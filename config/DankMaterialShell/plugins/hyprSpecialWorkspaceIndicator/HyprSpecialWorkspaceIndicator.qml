import QtQuick
import Quickshell.Hyprland
import qs.Common
import qs.Services
import qs.Widgets
import qs.Modules.Plugins

PluginComponent {
    id: root

    layerNamespacePlugin: "hypr-special-workspace-indicator"

    property string eventSpecialWorkspaceName: ""

    readonly property string iconName: "bookmark"
    readonly property bool hyprlandActive: CompositorService.isHyprland
    readonly property var hyprMonitor: hyprlandActive ? ((parentScreen ? Hyprland.monitorFor(parentScreen) : null) || Hyprland.focusedMonitor) : null
    readonly property var monitorIpc: hyprMonitor ? (hyprMonitor.lastIpcObject || ({})) : ({})
    readonly property var specialWorkspaceData: monitorIpc.specialWorkspace ?? monitorIpc.specialworkspace ?? null
    readonly property int pillTextSize: Theme.barTextSize(root.barThickness, root.barConfig?.fontScale)
    readonly property int verticalPillTextSize: Math.max(10, pillTextSize - 2)

    readonly property string specialWorkspaceName: resolveSpecialWorkspaceName()
    readonly property bool hasSpecialWorkspace: hyprlandActive && specialWorkspaceName.length > 0

    function normalizeSpecialWorkspaceName(raw) {
        let name = String(raw ?? "").trim();
        if (!name || name === "null" || name === "none")
            return "";
        if (name.startsWith("special:"))
            name = name.slice("special:".length);
        return name;
    }

    function currentMonitorName() {
        return String(hyprMonitor?.name ?? "").trim();
    }

    function currentMonitorId() {
        return hyprMonitor?.id;
    }

    function workspaceBelongsToCurrentMonitor(ws) {
        const wsMonitor = ws?.monitor;
        if (!wsMonitor)
            return false;

        const monitorId = currentMonitorId();
        const monitorName = currentMonitorName();

        if (monitorId !== undefined && monitorId !== null && wsMonitor.id === monitorId)
            return true;
        if (monitorName && wsMonitor.name === monitorName)
            return true;
        return false;
    }

    function specialWorkspaceFromMonitorIpc() {
        const sw = specialWorkspaceData;
        if (typeof sw === "string")
            return normalizeSpecialWorkspaceName(sw);
        if (sw && typeof sw === "object")
            return normalizeSpecialWorkspaceName(sw.name ?? "");
        return "";
    }

    function getSpecialWorkspaces() {
        return Array.from(Hyprland.workspaces?.values || []).filter(ws => ws && String(ws.name ?? "").startsWith("special:"));
    }

    function specialWorkspaceFromWorkspaceList() {
        if (!hyprlandActive)
            return "";

        const specialWorkspaces = getSpecialWorkspaces();
        if (specialWorkspaces.length === 0)
            return "";

        const monitorScoped = specialWorkspaces.filter(workspaceBelongsToCurrentMonitor);
        const pool = monitorScoped.length > 0 ? monitorScoped : specialWorkspaces;
        const activeWorkspace = pool.find(ws => ws.active || ws.focused);
        return activeWorkspace ? normalizeSpecialWorkspaceName(activeWorkspace.name ?? "") : "";
    }

    function snapshotSpecialWorkspaceName() {
        return specialWorkspaceFromMonitorIpc() || specialWorkspaceFromWorkspaceList();
    }

    function seedEventStateFromSnapshot() {
        eventSpecialWorkspaceName = snapshotSpecialWorkspaceName();
    }

    function resolveSpecialWorkspaceName() {
        return normalizeSpecialWorkspaceName(eventSpecialWorkspaceName)
            || specialWorkspaceFromMonitorIpc()
            || specialWorkspaceFromWorkspaceList();
    }

    function parseActivespecialEvent(event) {
        let parts = [];

        try {
            parts = event?.parse ? event.parse(2) : [];
        } catch (_) {
            parts = [];
        }

        if (!parts || parts.length === 0)
            parts = String(event?.data ?? "").split(",");

        return {
            workspaceName: String(parts[0] ?? "").trim(),
            monitorName: String(parts[1] ?? "").trim()
        };
    }

    function handleRawHyprEvent(event) {
        if (!event || event.name !== "activespecial")
            return;

        const parsed = parseActivespecialEvent(event);
        const thisMonitorName = currentMonitorName();

        if (parsed.monitorName && thisMonitorName && parsed.monitorName !== thisMonitorName)
            return;

        eventSpecialWorkspaceName = normalizeSpecialWorkspaceName(parsed.workspaceName);
    }

    Component.onCompleted: {
        root.setVisibilityOverride(root.hasSpecialWorkspace);
        // Plugin may start after the last activespecial event, so seed from current Hyprland state.
        seedEventStateFromSnapshot();
    }

    onHasSpecialWorkspaceChanged: root.setVisibilityOverride(root.hasSpecialWorkspace)
    onHyprMonitorChanged: seedEventStateFromSnapshot()

    Connections {
        target: Hyprland
        enabled: root.hyprlandActive

        function onRawEvent(event) {
            root.handleRawHyprEvent(event);
        }
    }

    horizontalBarPill: Component {
        Row {
            spacing: Theme.spacingXS
            visible: root.hasSpecialWorkspace

            DankIcon {
                anchors.verticalCenter: parent.verticalCenter
                name: root.iconName
                size: root.pillTextSize
                color: Theme.surfaceTextMedium
                weight: 500
            }

            StyledText {
                anchors.verticalCenter: parent.verticalCenter
                text: root.specialWorkspaceName
                color: Theme.surfaceText
                font.pixelSize: root.pillTextSize
                font.weight: Font.DemiBold
            }
        }
    }

    verticalBarPill: Component {
        Column {
            spacing: 2
            visible: root.hasSpecialWorkspace

            DankIcon {
                anchors.horizontalCenter: parent.horizontalCenter
                name: root.iconName
                size: root.pillTextSize
                color: Theme.surfaceTextMedium
                weight: 500
            }

            StyledText {
                anchors.horizontalCenter: parent.horizontalCenter
                text: root.specialWorkspaceName.length > 0 ? root.specialWorkspaceName.charAt(0).toUpperCase() : ""
                color: Theme.surfaceText
                font.pixelSize: root.verticalPillTextSize
                font.weight: Font.DemiBold
            }
        }
    }
}
