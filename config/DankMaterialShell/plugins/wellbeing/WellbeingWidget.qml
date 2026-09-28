// Today's screen time in the DankMaterialShell bar, from dotfiles'
// tools/wellbeing (`wellbeing --status`, the same JSON windots' YASB widget
// shows): a timer, or a moon while focus mode is on. Click shows the top
// app, middle-click toggles focus mode, right-click opens the dashboard.
import QtQuick
import Quickshell
import qs.Common
import qs.Widgets
import qs.Modules.Plugins

PluginComponent {
    id: root

    readonly property string wellbeing: Quickshell.env("HOME") + "/.local/bin/wellbeing"
    readonly property string dashboard: "http://127.0.0.1:5600/pages/wellbeing/"
    property string total: "--"
    property string topApp: ""
    property bool focusMode: false
    property bool showTopApp: false

    function refresh() {
        Proc.runCommand("wellbeing.status", [root.wellbeing, "--status"], (stdout, exitCode) => {
            if (exitCode !== 0)
                return;
            try {
                const status = JSON.parse(stdout);
                root.total = status.total || "--";
                root.topApp = status.top || "";
                root.focusMode = status.focus === "on";
            } catch (error) {
            }
        }, 100, undefined, root);
    }

    // The helper writes the new status as soon as it toggles; read it back.
    function toggleFocus() {
        Quickshell.execDetached([root.wellbeing, "--focus"]);
        refreshSoon.restart();
    }

    function click(mouse) {
        if (mouse.button === Qt.MiddleButton)
            toggleFocus();
        else if (mouse.button === Qt.RightButton)
            Quickshell.execDetached(["xdg-open", root.dashboard]);
        else
            root.showTopApp = !root.showTopApp;
    }

    // As often as the helper refreshes its usage.
    Timer {
        interval: 60000
        running: true
        repeat: true
        triggeredOnStart: true
        onTriggered: root.refresh()
    }

    Timer {
        id: refreshSoon
        interval: 400
        onTriggered: root.refresh()
    }

    horizontalBarPill: Component {
        Item {
            implicitWidth: row.implicitWidth
            implicitHeight: row.implicitHeight

            Row {
                id: row
                spacing: Theme.spacingXS

                DankIcon {
                    name: root.focusMode ? "bedtime" : "timer"
                    size: root.iconSize
                    color: root.focusMode ? Theme.primary : Theme.surfaceText
                    anchors.verticalCenter: parent.verticalCenter
                }

                StyledText {
                    text: root.showTopApp && root.topApp ? root.total + " · " + root.topApp : root.total
                    color: Theme.surfaceText
                    anchors.verticalCenter: parent.verticalCenter
                }
            }

            MouseArea {
                anchors.fill: parent
                acceptedButtons: Qt.LeftButton | Qt.MiddleButton | Qt.RightButton
                cursorShape: Qt.PointingHandCursor
                onClicked: mouse => root.click(mouse)
            }
        }
    }

    verticalBarPill: Component {
        Item {
            implicitWidth: column.implicitWidth
            implicitHeight: column.implicitHeight

            Column {
                id: column
                spacing: Theme.spacingXS

                DankIcon {
                    name: root.focusMode ? "bedtime" : "timer"
                    size: root.iconSize
                    color: root.focusMode ? Theme.primary : Theme.surfaceText
                    anchors.horizontalCenter: parent.horizontalCenter
                }

                StyledText {
                    text: root.total
                    color: Theme.surfaceText
                    anchors.horizontalCenter: parent.horizontalCenter
                }
            }

            MouseArea {
                anchors.fill: parent
                acceptedButtons: Qt.LeftButton | Qt.MiddleButton | Qt.RightButton
                cursorShape: Qt.PointingHandCursor
                onClicked: mouse => root.click(mouse)
            }
        }
    }
}
