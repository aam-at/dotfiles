#!/usr/bin/env bash
# Set up Whisper on OpenVINO for scripts/toggle-dictation.sh. whisper_ov.py runs
# under `uv run`, which caches its own Python environment (no venv to manage), so
# this only downloads that environment and the pre-converted large-v3-turbo int8
# model from the OpenVINO collection on Hugging Face (about 1 GB), then warms the
# compiled-model cache.
#
# The first NPU compile takes minutes; later starts load the cache in seconds,
# and whisper_ov.py then keeps the model loaded in a background server.
# NPU/GPU need Intel's drivers (intel-npu-driver / intel compute runtime) and
# your user in the `render` group. Once this has run, toggle-dictation.sh
# prefers it over whisper.cpp.

set -euo pipefail

TOOL="$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")/../scripts/whisper_ov.py"

uv run --quiet "$TOOL" --download

# One second of silence is enough to compile every graph: the server loads the
# model before it applies the silence gate. The first NPU compile takes minutes.
silence="$(mktemp --suffix=.wav)"
trap 'rm -f "$silence"' EXIT
python3 -c 'import sys,wave; w=wave.open(sys.argv[1],"wb"); w.setparams((1,2,16000,0,"NONE","")); w.writeframes(bytes(32000))' "$silence"
echo "Warming the compiled-model cache..."
python3 "$TOOL" "$silence" >/dev/null
