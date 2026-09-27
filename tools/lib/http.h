/*
 * A minimal HTTP client for aw-server on 127.0.0.1, over plain sockets on
 * both Windows (Winsock; link ws2_32) and Linux. HTTP/1.0, so the server
 * closes the connection after the response and never chunks it.
 *
 * Part of tools/lib, the header-only C library shared by dotfiles' tools
 * (wellbeing) and windots' native helpers (window-watcher, built with this
 * folder on the include path).
 */
#ifndef WELLBEING_HTTP_H
#define WELLBEING_HTTP_H

#include <stdio.h>
#include <string.h>

#ifdef _WIN32
#include <winsock2.h>
#include <ws2tcpip.h>
typedef SOCKET socket_t;
#define close_socket closesocket
#else
#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <unistd.h>
typedef int socket_t;
#define INVALID_SOCKET (-1)
#define close_socket close
#endif

static int http_port = 5600;

/* One request; returns the HTTP status (0 if aw-server is unreachable) and
   leaves the body, NUL-terminated, in response (if given). */
static int http_request(const char *method, const char *path, const char *body, char *response, size_t response_size) {
#ifdef _WIN32
    static int started;
    if (!started) {
        WSADATA data;
        started = WSAStartup(MAKEWORD(2, 2), &data) == 0;
    }
#endif
    socket_t s = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    if (s == INVALID_SOCKET) return 0;
#ifdef _WIN32
    DWORD timeout = 3000;
#else
    struct timeval timeout = {3, 0};
#endif
    setsockopt(s, SOL_SOCKET, SO_RCVTIMEO, (const char *)&timeout, sizeof timeout);
    setsockopt(s, SOL_SOCKET, SO_SNDTIMEO, (const char *)&timeout, sizeof timeout);
    /* 127.0.0.1, not localhost: aw-server listens on IPv4 only. */
    struct sockaddr_in address = {0};
    address.sin_family = AF_INET;
    address.sin_port = htons((unsigned short)http_port);
    address.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    if (connect(s, (struct sockaddr *)&address, sizeof address) != 0) {
        close_socket(s);
        return 0;
    }

    char head[1024];
    size_t length = body ? strlen(body) : 0;
    int head_length = snprintf(head, sizeof head,
        "%s %s HTTP/1.0\r\nHost: 127.0.0.1:%d\r\nContent-Type: application/json\r\nContent-Length: %zu\r\n\r\n",
        method, path, http_port, length);
    int sent = send(s, head, head_length, 0) == head_length;
    for (size_t done = 0; sent && done < length;) {
        int n = send(s, body + done, (int)(length - done), 0);
        if (n <= 0) sent = 0;
        else done += (size_t)n;
    }

    /* The whole response: status line, headers, body. */
    static char buffer[1 << 18];
    size_t total = 0;
    int n;
    while (sent && total + 1 < sizeof buffer && (n = recv(s, buffer + total, (int)(sizeof buffer - 1 - total), 0)) > 0)
        total += (size_t)n;
    close_socket(s);
    buffer[total] = 0;

    int status = 0;
    if (sscanf(buffer, "HTTP/%*d.%*d %d", &status) != 1) return 0;
    if (response && response_size) {
        const char *start = strstr(buffer, "\r\n\r\n");
        start = start ? start + 4 : buffer + total;
        snprintf(response, response_size, "%s", start);
    }
    return status;
}

#endif
