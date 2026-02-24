#!/bin/bash
# Push named workspaces to the end of each output's workspace list.
# This keeps low indices (1-9) free for regular unnamed workspaces.
# Run once at startup after niri creates the initial workspace layout.

sleep 2  # wait for niri to finish initializing

NAMED_WORKSPACES=(sysmon music communication ai todo)

# Move each named workspace to the very end, in order.
# Using 999 as target index - niri clamps it to the actual max position.
for ws in "${NAMED_WORKSPACES[@]}"; do
  niri msg action move-workspace-to-index --reference "$ws" 999 2>/dev/null
done
