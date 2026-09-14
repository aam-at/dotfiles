#!/usr/bin/env bash
# Build whisper.cpp from source for scripts/toggle-dictation.sh, tuned for
# this machine, into ~/.local/src/whisper.cpp/build-<backend>, symlinked as
# ~/.local/bin/whisper-cli-<backend>. Replaces the pacman whisper-cpp
# package entirely: a "cpu" build already uses -march=native (the pacman
# package targets a generic baseline arch), and a GPU build (cuda/sycl/
# vulkan) still runs fine with no GPU present — ggml falls back to its
# built-in CPU backend at runtime, so one binary covers both cases.
#
# Usage: install_whisper_cpp_gpu.sh [cuda|sycl|vulkan|cpu|auto]  (default: auto)
#
#   cuda   - NVIDIA GPUs. Needs the `cuda` package (nvcc) installed.
#   sycl   - Intel GPUs (Arc, Xe, built-in Arc iGPU in Meteor Lake/Panther Lake).
#            Needs Intel's oneAPI base toolkit installed first (see below) —
#            this script does not install it, since it's a multi-GB download
#            that needs its own review.
#   vulkan - any GPU with a Vulkan driver; the easy fallback for Intel/AMD
#            iGPUs when you don't want to install oneAPI.
#   cpu    - no GPU backend, just a native-tuned build (-march=native).
#   auto   - nvidia-smi present -> cuda; else an Intel GPU is present and
#            oneAPI is already installed -> sycl; else a Vulkan-capable GPU
#            is present -> vulkan; else cpu.
#
# Re-run this after a `git pull` in the source checkout to rebuild with newer
# whisper.cpp; it reuses the existing clone and reconfigures in place.

set -euo pipefail

BACKEND="${1:-auto}"
SRC_DIR="$HOME/.local/src/whisper.cpp"
BIN_DIR="$HOME/.local/bin"
JOBS="${DICTATION_BUILD_JOBS:-4}" # CUDA/SYCL compiles are memory-hungry per job

detect_backend() {
  if command -v nvidia-smi >/dev/null 2>&1 && nvidia-smi -L >/dev/null 2>&1; then
    echo cuda
    return
  fi
  if [[ -f /opt/intel/oneapi/setvars.sh ]] && lspci -nn 2>/dev/null | grep -qiE "(VGA|3D|Display).*Intel"; then
    echo sycl
    return
  fi
  if command -v vulkaninfo >/dev/null 2>&1 && vulkaninfo --summary >/dev/null 2>&1; then
    echo vulkan
    return
  fi
  echo none
}

if [[ "$BACKEND" == "auto" ]]; then
  BACKEND="$(detect_backend)"
  [[ "$BACKEND" == "none" ]] && BACKEND=cpu
  echo "Auto-detected backend: $BACKEND"
fi

case "$BACKEND" in
cuda)
  if ! command -v nvcc >/dev/null 2>&1; then
    echo "nvcc not found. Install the CUDA toolkit first: sudo pacman -S cuda" >&2
    exit 1
  fi
  CMAKE_ARGS=(-DGGML_CUDA=ON -DCMAKE_BUILD_TYPE=Release)
  ;;
sycl)
  if [[ ! -f /opt/intel/oneapi/setvars.sh ]]; then
    cat >&2 <<'EOF'
Intel oneAPI base toolkit not found at /opt/intel/oneapi/setvars.sh.

Install it first (AUR: intel-oneapi-basekit — several GB), then re-run this
script. See README_sycl.md in the whisper.cpp source checkout for the full
setup (GPU driver, video/render groups, oneAPI) before building.
EOF
    exit 1
  fi
  # setvars.sh can return non-zero if it's already been sourced in this
  # shell (e.g. re-running this script, or a shell rc that also sources
  # it) even though the environment is already set up correctly, and can
  # also reference variables of its own that aren't set yet — neither
  # should be fatal here, but `set -u` will kill the whole script on an
  # unbound-variable reference regardless of `|| true`, so disable it
  # just for this line.
  set +u
  # shellcheck disable=SC1091
  source /opt/intel/oneapi/setvars.sh || true
  set -u
  CMAKE_ARGS=(-DGGML_SYCL=ON -DCMAKE_C_COMPILER=icx -DCMAKE_CXX_COMPILER=icpx -DCMAKE_BUILD_TYPE=Release)
  ;;
vulkan)
  if ! command -v vulkaninfo >/dev/null 2>&1; then
    echo "vulkaninfo not found. Install a Vulkan loader first: sudo pacman -S vulkan-icd-loader vulkan-tools" >&2
    exit 1
  fi
  CMAKE_ARGS=(-DGGML_VULKAN=ON -DCMAKE_BUILD_TYPE=Release)
  ;;
cpu)
  CMAKE_ARGS=(-DCMAKE_BUILD_TYPE=Release)
  ;;
*)
  echo "Unknown backend: $BACKEND (expected cuda, sycl, vulkan, cpu, or auto)" >&2
  exit 1
  ;;
esac

mkdir -p "$(dirname "$SRC_DIR")" "$BIN_DIR"
if [[ -d "$SRC_DIR/.git" ]]; then
  git -C "$SRC_DIR" pull --ff-only
else
  git clone --depth 1 https://github.com/ggml-org/whisper.cpp.git "$SRC_DIR"
fi

BUILD_DIR="build-$BACKEND"
cmake -S "$SRC_DIR" -B "$SRC_DIR/$BUILD_DIR" "${CMAKE_ARGS[@]}"
cmake --build "$SRC_DIR/$BUILD_DIR" -j"$JOBS" --config Release --target whisper-cli

ln -sf "$SRC_DIR/$BUILD_DIR/bin/whisper-cli" "$BIN_DIR/whisper-cli-$BACKEND"
echo "Built and linked $BIN_DIR/whisper-cli-$BACKEND"
echo "toggle-dictation.sh will pick it up automatically."
