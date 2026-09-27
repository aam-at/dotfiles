/*
 * wellbeing on Windows: nudges are a popup under the bar (Windows toasts
 * would be hidden by Do Not Disturb), focus changes come from WinEvents,
 * idle time from GetLastInputInfo, and grayscale from the Magnification API
 * (lasting while this process runs). Do Not Disturb defaults to windots'
 * scripts\Toggle-Dnd.ps1. The log and the bar's status file go with
 * windots' other state, in %LOCALAPPDATA%\windots.
 */
#include <windows.h>
#include <dwmapi.h>
#include <magnification.h>
#include <wchar.h>

/* Segoe Fluent Stopwatch (U+E916) and QuietHours (U+E708), in UTF-8. */
#define PLATFORM_ICON_TIMER "\xEE\xA4\x96"
#define PLATFORM_ICON_FOCUS "\xEE\x9C\x88"

#define WM_APP_FOCUS (WM_APP + 1)
#define WM_APP_BEDTIME (WM_APP + 2)
#define TIMER_SETTINGS 1
#define TIMER_USAGE 2
#define TIMER_FOCUS_CHECK 3
#define TIMER_POPUP 4
#define TIMER_FADE 5
#define TIMER_SYNC_SOON 6
#define POPUP_MS 6000
#define POPUP_ALPHA 245

static void core_settings(void);
static void core_usage(void);
static void core_check_focus(void);
static void core_start(void);
static void core_stop(void);

static HWND command_window, popup_window;
static wchar_t popup_title[160], popup_body[320];
static double popup_progress = -1;
static int popup_alpha, popup_fading_in;
static HWINEVENTHOOK focus_hook, title_hook;
static DWORD title_hook_pid;
static int focus_check_pending;

static double platform_seconds(void) {
    return GetTickCount64() / 1000.0;
}

static double platform_idle_seconds(void) {
    LASTINPUTINFO input = {sizeof input};
    return GetLastInputInfo(&input) ? (GetTickCount() - input.dwTime) / 1000.0 : -1;
}

static int platform_foreground(char *app, size_t size) {
    wchar_t path[MAX_PATH] = L"";
    DWORD pid = 0, length = MAX_PATH;
    GetWindowThreadProcessId(GetForegroundWindow(), &pid);
    HANDLE process = OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, FALSE, pid);
    if (process) QueryFullProcessImageNameW(process, 0, path, &length), CloseHandle(process);
    const wchar_t *name = wcsrchr(path, L'\\');
    WideCharToMultiByte(CP_UTF8, 0, name ? name + 1 : path, -1, app, (int)size, NULL, NULL);
    return 1;
}

static void platform_data_path(const char *name, char *out, size_t size) {
    char base[MAX_PATH];
    GetEnvironmentVariableA("LOCALAPPDATA", base, MAX_PATH);
    snprintf(out, size, "%s\\windots", base);
    CreateDirectoryA(out, NULL);
    snprintf(out, size, "%s\\windots\\%s", base, name);
}

static void platform_write_stdout(const char *text) {
    DWORD written;
    WriteFile(GetStdHandle(STD_OUTPUT_HANDLE), text, (DWORD)strlen(text), &written, NULL);
}

/* ---- Commands ---- */

/* windots: %WINDOTS%, else ~\windots, if Toggle-Dnd.ps1 is there. */
static int windots_dir(char *out, size_t size) {
    char script[MAX_PATH + 32];
    if (!GetEnvironmentVariableA("WINDOTS", out, (DWORD)size)) {
        GetEnvironmentVariableA("USERPROFILE", out, (DWORD)size);
        snprintf(out + strlen(out), size - strlen(out), "\\windots");
    }
    snprintf(script, sizeof script, "%s\\scripts\\Toggle-Dnd.ps1", out);
    return GetFileAttributesA(script) != INVALID_FILE_ATTRIBUTES;
}

static void platform_default_commands(Config *c) {
    char windots[MAX_PATH];
    if (!windots_dir(windots, sizeof windots)) return;
    const char *dnd = "powershell.exe -NoProfile -ExecutionPolicy Bypass -File \"%s\\scripts\\Toggle-Dnd.ps1\" -State %s";
    if (!*c->dnd_on) snprintf(c->dnd_on, sizeof c->dnd_on, dnd, windots, "On");
    if (!*c->dnd_off) snprintf(c->dnd_off, sizeof c->dnd_off, dnd, windots, "Off");
}

/* A shell command (cmd.exe; %VARIABLES% expand), with no window. */
static void platform_run(const char *command) {
    wchar_t wide[COMMAND_SIZE], line[COMMAND_SIZE + 32];
    MultiByteToWideChar(CP_UTF8, 0, command, -1, wide, COMMAND_SIZE);
    swprintf(line, COMMAND_SIZE + 32, L"cmd.exe /d /c %ls", wide);
    STARTUPINFOW startup = {sizeof startup};
    PROCESS_INFORMATION process;
    if (CreateProcessW(NULL, line, NULL, NULL, FALSE, CREATE_NO_WINDOW, NULL, NULL, &startup, &process)) {
        CloseHandle(process.hThread);
        CloseHandle(process.hProcess);
    }
}

/* Magnification.dll's full-screen colour effect. MinGW has no import
   library for it, hence GetProcAddress. */
static int platform_grayscale(int on) {
    static BOOL (WINAPI *set_effect)(PMAGCOLOREFFECT);
    if (!set_effect) {
        HMODULE mag = LoadLibraryW(L"Magnification.dll");
        BOOL (WINAPI *initialize)(void) = mag ? (void *)GetProcAddress(mag, "MagInitialize") : NULL;
        if (!initialize || !initialize()) return 0;
        set_effect = (void *)GetProcAddress(mag, "MagSetFullscreenColorEffect");
        if (!set_effect) return 0;
    }
    MAGCOLOREFFECT gray = {{{0.3f, 0.3f, 0.3f, 0, 0}, {0.6f, 0.6f, 0.6f, 0, 0}, {0.1f, 0.1f, 0.1f, 0, 0}, {0, 0, 0, 1, 0}, {0, 0, 0, 0, 1}}};
    MAGCOLOREFFECT identity = {{{1, 0, 0, 0, 0}, {0, 1, 0, 0, 0}, {0, 0, 1, 0, 0}, {0, 0, 0, 1, 0}, {0, 0, 0, 0, 1}}};
    set_effect(on ? &gray : &identity);
    return 1;
}

/* ---- The popup ---- */

static void platform_notify(const char *title, const char *body, double progress, int urgent) {
    (void)urgent; /* the popup shows through Do Not Disturb anyway */
    MultiByteToWideChar(CP_UTF8, 0, title, -1, popup_title, 160);
    MultiByteToWideChar(CP_UTF8, 0, body, -1, popup_body, 320);
    popup_progress = progress;
    /* Under the bar, on the monitor in use. */
    HMONITOR monitor = MonitorFromWindow(GetForegroundWindow(), MONITOR_DEFAULTTOPRIMARY);
    MONITORINFO info = {sizeof info};
    GetMonitorInfoW(monitor, &info);
    UINT dpi = 96, dpi_y;
    HMODULE shcore = LoadLibraryW(L"Shcore.dll");
    HRESULT (WINAPI *get_dpi)(HMONITOR, int, UINT *, UINT *) = shcore ? (void *)GetProcAddress(shcore, "GetDpiForMonitor") : NULL;
    if (get_dpi) get_dpi(monitor, 0, &dpi, &dpi_y);
    int width = MulDiv(400, dpi, 96), height = MulDiv(progress >= 0 ? 76 : 64, dpi, 96);
    int x = (info.rcWork.left + info.rcWork.right - width) / 2, y = info.rcWork.top + MulDiv(10, dpi, 96);
    if (!IsWindowVisible(popup_window)) popup_alpha = 0;
    SetLayeredWindowAttributes(popup_window, 0, (BYTE)popup_alpha, LWA_ALPHA);
    SetWindowPos(popup_window, HWND_TOPMOST, x, y, width, height, SWP_NOACTIVATE | SWP_SHOWWINDOW);
    InvalidateRect(popup_window, NULL, FALSE);
    popup_fading_in = 1;
    SetTimer(popup_window, TIMER_FADE, 15, NULL);
    SetTimer(popup_window, TIMER_POPUP, POPUP_MS, NULL);
}

static HFONT popup_font(int bold, UINT dpi) {
    static HFONT cached[2];
    static UINT cached_dpi[2];
    if (!cached[bold] || cached_dpi[bold] != dpi) {
        if (cached[bold]) DeleteObject(cached[bold]);
        cached[bold] = CreateFontW(-MulDiv(bold ? 15 : 13, dpi, 96), 0, 0, 0, bold ? FW_SEMIBOLD : FW_NORMAL, 0, 0, 0, DEFAULT_CHARSET, 0, 0,
            CLEARTYPE_QUALITY, 0, bold ? L"Segoe UI Variable Display" : L"Segoe UI Variable Text");
        cached_dpi[bold] = dpi;
    }
    return cached[bold];
}

static void fill(HDC dc, RECT rect, COLORREF color) {
    HBRUSH brush = CreateSolidBrush(color);
    FillRect(dc, &rect, brush);
    DeleteObject(brush);
}

static void paint_popup(HWND hwnd, HDC dc) {
    RECT rect;
    GetClientRect(hwnd, &rect);
    UINT dpi = GetDpiForWindow(hwnd);
    int pad = MulDiv(16, dpi, 96);
    /* The bar's gruvbox: surface, yellow title, cream text. */
    fill(dc, rect, RGB(0x32, 0x30, 0x2f));
    SetBkMode(dc, TRANSPARENT);
    RECT line = {rect.left + pad, rect.top + MulDiv(10, dpi, 96), rect.right - pad, rect.top + MulDiv(34, dpi, 96)};
    SelectObject(dc, popup_font(1, dpi));
    SetTextColor(dc, RGB(0xfa, 0xbd, 0x2f));
    DrawTextW(dc, popup_title, -1, &line, DT_SINGLELINE | DT_END_ELLIPSIS | DT_NOPREFIX);
    line.top = line.bottom, line.bottom = line.top + MulDiv(22, dpi, 96);
    SelectObject(dc, popup_font(0, dpi));
    SetTextColor(dc, RGB(0xeb, 0xdb, 0xb2));
    DrawTextW(dc, popup_body, -1, &line, DT_SINGLELINE | DT_END_ELLIPSIS | DT_NOPREFIX);
    if (popup_progress < 0) return;
    /* Use against the limit: blue, then red once over it. */
    int top = rect.bottom - MulDiv(14, dpi, 96), height = MulDiv(4, dpi, 96);
    RECT track = {rect.left + pad, top, rect.right - pad, top + height};
    fill(dc, track, RGB(0x45, 0x40, 0x3d));
    double shown = popup_progress > 1 ? 1 : popup_progress;
    RECT used = track;
    used.right = track.left + (LONG)((track.right - track.left) * shown);
    fill(dc, used, popup_progress >= 1 ? RGB(0xfb, 0x49, 0x34) : RGB(0x4f, 0x9b, 0xd9));
}

static LRESULT CALLBACK popup_proc(HWND hwnd, UINT message, WPARAM wparam, LPARAM lparam) {
    if (message == WM_PAINT) {
        /* Drawn off screen, then copied: no flicker. */
        PAINTSTRUCT ps;
        HDC dc = BeginPaint(hwnd, &ps);
        RECT rect;
        GetClientRect(hwnd, &rect);
        HDC memory = CreateCompatibleDC(dc);
        HBITMAP bitmap = CreateCompatibleBitmap(dc, rect.right, rect.bottom);
        HGDIOBJ old = SelectObject(memory, bitmap);
        paint_popup(hwnd, memory);
        BitBlt(dc, 0, 0, rect.right, rect.bottom, memory, 0, 0, SRCCOPY);
        SelectObject(memory, old);
        DeleteObject(bitmap);
        DeleteDC(memory);
        EndPaint(hwnd, &ps);
        return 0;
    }
    if (message == WM_ERASEBKGND) return 1;
    if (message == WM_TIMER && wparam == TIMER_FADE) {
        popup_alpha += popup_fading_in ? 49 : -35;
        if (popup_alpha >= POPUP_ALPHA) popup_alpha = POPUP_ALPHA, KillTimer(hwnd, TIMER_FADE);
        if (popup_alpha <= 0) popup_alpha = 0, KillTimer(hwnd, TIMER_FADE), ShowWindow(hwnd, SW_HIDE);
        SetLayeredWindowAttributes(hwnd, 0, (BYTE)popup_alpha, LWA_ALPHA);
        return 0;
    }
    if ((message == WM_TIMER && wparam == TIMER_POPUP) || message == WM_LBUTTONDOWN) {
        KillTimer(hwnd, TIMER_POPUP);
        popup_fading_in = 0;
        SetTimer(hwnd, TIMER_FADE, 15, NULL);
        return 0;
    }
    return DefWindowProcW(hwnd, message, wparam, lparam);
}

/* ---- Focus changes ---- */

static void CALLBACK on_focus_event(HWINEVENTHOOK hook, DWORD event, HWND hwnd, LONG object, LONG child, DWORD thread, DWORD time);

/* A check 1.5 s after the change, once the browser extension has reported
   a new tab. Not restarted while pending: a title that changes every second
   (a spinner) would otherwise put it off forever. Title changes of the
   foreground process catch tab switches. */
static void schedule_focus_check(void) {
    if (!focus_check_pending) focus_check_pending = SetTimer(command_window, TIMER_FOCUS_CHECK, 1500, NULL) != 0;
    DWORD pid = 0;
    GetWindowThreadProcessId(GetForegroundWindow(), &pid);
    if (pid == title_hook_pid && title_hook) return;
    if (title_hook) UnhookWinEvent(title_hook);
    title_hook = SetWinEventHook(EVENT_OBJECT_NAMECHANGE, EVENT_OBJECT_NAMECHANGE, NULL, on_focus_event, pid, 0, WINEVENT_OUTOFCONTEXT);
    title_hook_pid = pid;
}

static void CALLBACK on_focus_event(HWINEVENTHOOK hook, DWORD event, HWND hwnd, LONG object, LONG child, DWORD thread, DWORD time) {
    if (event == EVENT_OBJECT_NAMECHANGE && (object != OBJID_WINDOW || hwnd != GetForegroundWindow())) return;
    schedule_focus_check();
}

static void platform_watch_focus(int on) {
    if (on && !focus_hook) {
        focus_hook = SetWinEventHook(EVENT_SYSTEM_FOREGROUND, EVENT_SYSTEM_FOREGROUND, NULL, on_focus_event, 0, 0, WINEVENT_OUTOFCONTEXT | WINEVENT_SKIPOWNPROCESS);
        schedule_focus_check();
    }
    if (!on) {
        if (focus_hook) UnhookWinEvent(focus_hook);
        if (title_hook) UnhookWinEvent(title_hook);
        focus_hook = title_hook = NULL;
        title_hook_pid = 0;
    }
}

/* ---- The running helper ---- */

static int platform_running(void) {
    return FindWindowExW(HWND_MESSAGE, NULL, L"wellbeing", NULL) != NULL;
}

static void platform_send(Command command) {
    HWND running = FindWindowExW(HWND_MESSAGE, NULL, L"wellbeing", NULL);
    if (running) PostMessageW(running, command == COMMAND_FOCUS ? WM_APP_FOCUS : WM_APP_BEDTIME, 0, 0);
}

/* After the popup has had time to show: a refused connection to aw-server
   takes Windows 2 s, and this thread also draws the popup. */
static void platform_sync_soon(void) {
    SetTimer(command_window, TIMER_SYNC_SOON, 500, NULL);
}

static LRESULT CALLBACK command_proc(HWND hwnd, UINT message, WPARAM wparam, LPARAM lparam) {
    if (message == WM_TIMER && wparam == TIMER_SETTINGS) core_settings();
    else if (message == WM_TIMER && wparam == TIMER_SYNC_SOON) KillTimer(hwnd, TIMER_SYNC_SOON), core_settings();
    else if (message == WM_TIMER && wparam == TIMER_USAGE) core_usage();
    else if (message == WM_TIMER && wparam == TIMER_FOCUS_CHECK) KillTimer(hwnd, TIMER_FOCUS_CHECK), focus_check_pending = 0, core_check_focus();
    else if (message == WM_APP_FOCUS) core_toggle_focus();
    else if (message == WM_APP_BEDTIME) core_toggle_bedtime();
    else if (message == WM_ENDSESSION && wparam) core_stop();
    else return DefWindowProcW(hwnd, message, wparam, lparam);
    return 0;
}

static int platform_daemon(void) {
    CreateMutexW(NULL, FALSE, L"wellbeing");
    if (GetLastError() == ERROR_ALREADY_EXISTS) return 0;
    SetProcessDpiAwarenessContext(DPI_AWARENESS_CONTEXT_PER_MONITOR_AWARE_V2);
    HINSTANCE instance = GetModuleHandleW(NULL);

    WNDCLASSW command_class = {.lpfnWndProc = command_proc, .hInstance = instance, .lpszClassName = L"wellbeing"};
    RegisterClassW(&command_class);
    command_window = CreateWindowExW(0, L"wellbeing", L"", 0, 0, 0, 0, 0, HWND_MESSAGE, NULL, instance, NULL);
    WNDCLASSW popup_class = {.lpfnWndProc = popup_proc, .hInstance = instance, .lpszClassName = L"wellbeing-popup", .hCursor = LoadCursor(NULL, IDC_HAND)};
    RegisterClassW(&popup_class);
    popup_window = CreateWindowExW(WS_EX_TOPMOST | WS_EX_TOOLWINDOW | WS_EX_NOACTIVATE | WS_EX_LAYERED, L"wellbeing-popup", L"Wellbeing", WS_POPUP, 0, 0, 0, 0, NULL, NULL, instance, NULL);
    DWORD round = 2; /* DWMWCP_ROUND */
    DwmSetWindowAttribute(popup_window, 33 /* DWMWA_WINDOW_CORNER_PREFERENCE */, &round, sizeof round);

    core_start();
    SetTimer(command_window, TIMER_SETTINGS, SETTINGS_EVERY_S * 1000, NULL);
    SetTimer(command_window, TIMER_USAGE, USAGE_EVERY_S * 1000, NULL);

    MSG message;
    while (GetMessageW(&message, NULL, 0, 0) > 0) DispatchMessageW(&message);
    core_stop();
    return 0;
}

static int wellbeing_main(int argc, char **argv);

int WINAPI WinMain(HINSTANCE instance, HINSTANCE previous, LPSTR command_line, int show) {
    return wellbeing_main(__argc, __argv);
}
