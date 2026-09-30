# /// script
# requires-python = ">=3.10,<3.14"
# dependencies = ["openvino-genai", "huggingface_hub"]
# ///
"""Whisper on OpenVINO GenAI, as a warm local server plus a thin client (Linux and Windows).

Used by toggle-dictation.sh on Linux (set up by setup/install_whisper_openvino.sh)
and windots' Toggle-Dictation.ps1 on Windows. The
client needs only the standard library; it starts the server through `uv run`,
which supplies OpenVINO in a cached environment.

  whisper_ov.py AUDIO       transcribe 16 kHz mono s16 audio (a WAV or raw s16le): text on stdout, timings
                            on stderr. Starts the server on first use.
  whisper_ov.py --start     make sure the server is up (loads the model while you talk)
  whisper_ov.py --stop      stop the server (frees the NPU/GPU)
  whisper_ov.py --serve     run the server here (the client spawns this via uv)
  uv run whisper_ov.py --download   fetch the pre-converted model and exit

Env: DICTATION_DEVICE (comma list tried in order, CPU always last; default NPU,GPU),
DICTATION_LANG (en), WHISPER_OV_IDLE (server idle exit, 1800 s), WHISPER_OV_PORT
(47600), WHISPER_OV_MIN_RMS (0.003; quieter audio is skipped because Whisper
invents text for near-silence). Compiled NPU/GPU blobs are cached beside the model;
the first NPU compile takes minutes.
"""
import argparse
import json
import os
import socket
import subprocess
import sys
import time
import wave
from pathlib import Path

HOME = Path.home() / ".local" / "share" / "whisper-ov"
MODEL = "OpenVINO/whisper-large-v3-turbo-int8-ov"
ADDR = ("127.0.0.1", int(os.environ.get("WHISPER_OV_PORT", 47600)))
LOG = (
    Path(os.environ["LOCALAPPDATA"]) / "windots" / "dictation"
    if os.name == "nt"
    else Path(os.environ.get("XDG_RUNTIME_DIR", "/tmp")) / "dictation"
) / "whisper-server.log"

PIPE = None  # (device, pipeline)
BAD = set()  # devices that failed to load


def pipe_for(model, spec):
    """(pipeline, device) for the first usable device in `spec`, else the CPU."""
    global PIPE
    import openvino as ov
    import openvino_genai

    have = {d.split(".")[0] for d in ov.Core().available_devices}
    for d in dict.fromkeys([*spec.upper().split(","), "CPU"]):
        if d not in have or d in BAD:
            continue
        if PIPE and PIPE[0] == d:
            return PIPE[1], d
        try:
            PIPE = None  # free the old model before loading another
            t = time.perf_counter()
            pipe = openvino_genai.WhisperPipeline(model, d, CACHE_DIR=str(Path(model) / f"cache-{d}"))
            print(f"loaded {d} in {time.perf_counter() - t:.1f}s", flush=True)
            PIPE = (d, pipe)
            return pipe, d
        except Exception as e:
            BAD.add(d)
            print(f"{d} unavailable ({type(e).__name__}: {e})", flush=True)
    raise RuntimeError(f"no usable OpenVINO device (have {sorted(have)})")


def transcribe(pipe, pcm, lang):
    import numpy as np

    audio = np.frombuffer(pcm, dtype="<i2").astype(np.float32) / 32768.0
    seconds = len(audio) / 16000
    if not len(audio) or np.sqrt(np.mean(audio**2)) < float(os.environ.get("WHISPER_OV_MIN_RMS", 0.003)):
        return "", seconds
    cfg = pipe.get_generation_config()
    cfg.language = f"<|{lang}|>"
    cfg.task = "transcribe"
    return pipe.generate(audio.tolist(), cfg).texts[0].strip(), seconds


def read_pcm(path):
    if not str(path).endswith(".wav"):
        return Path(path).read_bytes()
    with wave.open(str(path)) as w:
        if (w.getframerate(), w.getnchannels(), w.getsampwidth()) != (16000, 1, 2):
            sys.exit("expected 16 kHz mono 16-bit audio")
        return w.readframes(w.getnframes())


def serve(a):
    srv = socket.socket()
    if os.name != "nt":  # on Windows SO_REUSEADDR would allow a second bind
        srv.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    try:
        srv.bind(ADDR)
    except OSError:
        return  # another server won the race
    srv.listen(8)
    srv.settimeout(int(os.environ.get("WHISPER_OV_IDLE", 1800)))
    # Clients that connect while this loads wait in the backlog.
    pipe_for(a.model, a.device)
    while True:
        try:
            conn, _ = srv.accept()
        except socket.timeout:
            return
        with conn, conn.makefile("rwb") as f:
            try:
                line = f.readline()
                if not line:  # a bare probe from up()
                    continue
                head = json.loads(line)
                if head.get("cmd") == "quit":
                    return
                pcm = f.read(head["bytes"])
                pipe, device = pipe_for(a.model, a.device)
                t = time.perf_counter()
                text, seconds = transcribe(pipe, pcm, head.get("lang", a.lang))
                reply = {"text": text, "audio": seconds, "generate": time.perf_counter() - t, "device": device}
            except Exception as e:  # report to the client, keep serving
                reply = {"error": f"{type(e).__name__}: {e}"}
            f.write(json.dumps(reply).encode() + b"\n")


def up():
    try:
        socket.create_connection(ADDR, timeout=1).close()
        return True
    except OSError:
        return False


def ensure_server(a):
    if up():
        return
    LOG.parent.mkdir(parents=True, exist_ok=True)
    kw = (
        # NEW_GROUP | NO_WINDOW: a hidden console that uv's child python inherits. DETACHED_PROCESS
        # would make NO_WINDOW void and give that child a visible console of its own.
        {"creationflags": 0x00000200 | 0x08000000}
        if os.name == "nt"
        else {"start_new_session": True}
    )
    subprocess.Popen(
        ["uv", "run", "--quiet", __file__, "--serve", "--device", a.device, "--model", a.model],
        stdin=subprocess.DEVNULL, stdout=open(LOG, "ab"), stderr=subprocess.STDOUT, **kw,
    )
    # The server binds before it loads, so this only waits for Python to start.
    for _ in range(100):
        if up():
            return
        time.sleep(0.1)
    sys.exit(f"whisper server did not start (see {LOG})")


def main():
    p = argparse.ArgumentParser()
    p.add_argument("audio", nargs="?")
    p.add_argument("--device", default=os.environ.get("DICTATION_DEVICE", "NPU,GPU"))
    p.add_argument("--model", default=str(HOME / "models" / MODEL.split("/")[1]))
    p.add_argument("--lang", default=os.environ.get("DICTATION_LANG", "en"))
    for flag in ("start", "stop", "serve", "download"):
        p.add_argument(f"--{flag}", action="store_true")
    a = p.parse_args()

    if a.download:
        from huggingface_hub import snapshot_download

        snapshot_download(MODEL, local_dir=a.model)
    elif a.serve:
        serve(a)
    elif a.start:
        ensure_server(a)
    elif a.stop:
        if up():
            with socket.create_connection(ADDR, timeout=5) as s:
                s.sendall(b'{"cmd": "quit"}\n')
    else:
        pcm = read_pcm(a.audio)
        ensure_server(a)
        with socket.create_connection(ADDR, timeout=5) as s, s.makefile("rwb") as f:
            s.settimeout(1200)  # a cold NPU compile can take minutes
            f.write(json.dumps({"bytes": len(pcm), "lang": a.lang}).encode() + b"\n" + pcm)
            f.flush()
            reply = json.loads(f.readline())
        if "error" in reply:
            sys.exit(reply["error"])
        print(reply["text"])
        print(f"device={reply['device']} audio={reply['audio']:.1f}s generate={reply['generate']:.2f}s", file=sys.stderr)


if __name__ == "__main__":
    main()
