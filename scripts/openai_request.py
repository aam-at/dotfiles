#!/usr/bin/env python
"""Send one chat request to an OpenAI-compatible API (OpenAI, DeepSeek, Gemini)."""

import argparse
import json
import os
import urllib.error
import urllib.request

try:
    from dotenv import load_dotenv

    load_dotenv()
except ImportError:
    pass

# provider: (chat completions URL, API key variable, default model)
PROVIDERS = {
    "openai": (
        "https://api.openai.com/v1/chat/completions",
        "OPENAI_API_KEY",
        "gpt-4o-mini",
    ),
    "deepseek": (
        "https://api.deepseek.com/v1/chat/completions",
        "DEEPSEEK_API_KEY",
        "deepseek-chat",
    ),
    "gemini": (
        "https://generativelanguage.googleapis.com/v1beta/openai/chat/completions",
        "GEMINI_API_KEY",
        "gemini-1.5-flash",
    ),
}


def build_payload(args, schema):
    system_prompt = args.system_prompt
    for path in args.context_files:
        try:
            with open(path) as f:
                system_prompt += (
                    f"\n\nRequest context in {os.path.basename(path)}:\n{f.read()}"
                )
        except OSError as e:
            print(f"Warning: Error reading file {path}: {e}")

    payload = {"model": args.model, "temperature": args.temperature}
    if schema is not None:
        if args.provider == "deepseek":
            # DeepSeek supports JSON mode but not JSON schemas.
            payload["response_format"] = {"type": "json_object"}
            system_prompt += f"\n{schema}"
        else:
            payload["response_format"] = {
                "type": "json_schema",
                "json_schema": {
                    "name": "json_response",
                    "strict": True,
                    "schema": schema,
                },
            }
    payload["messages"] = [
        {"role": "system", "content": system_prompt},
        {"role": "user", "content": args.user_prompt},
    ]
    return payload


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--provider", choices=PROVIDERS, default="openai")
    parser.add_argument(
        "--api_key", help="API key (default: the provider's *_API_KEY variable)"
    )
    parser.add_argument("--model", help="Model (default: the provider's default model)")
    parser.add_argument("--temperature", type=float, default=0.7)
    parser.add_argument(
        "--system_prompt",
        default="You are a large language model and a writing assistant. Respond concisely.",
    )
    parser.add_argument(
        "--context_files", nargs="*", default=[], help="Paths to context files"
    )
    parser.add_argument("--user_prompt", required=True)
    parser.add_argument("--response_format", help="JSON schema file for the response")
    args = parser.parse_args()

    url, key_var, default_model = PROVIDERS[args.provider]
    args.model = args.model or default_model
    api_key = args.api_key or os.getenv(key_var)

    schema = None
    if args.response_format:
        try:
            with open(args.response_format) as f:
                schema = json.load(f)
        except (OSError, json.JSONDecodeError) as e:
            print(f"Error reading response format file: {e}")
            return

    request = urllib.request.Request(
        url,
        data=json.dumps(build_payload(args, schema)).encode(),
        headers={
            "Authorization": f"Bearer {api_key}",
            "Content-Type": "application/json",
        },
    )
    try:
        with urllib.request.urlopen(request) as response:
            body = response.read().decode()
    except urllib.error.HTTPError as e:
        body = e.read().decode()
    except urllib.error.URLError as e:
        print(f"An error occurred: {e}")
        return

    choices = json.loads(body).get("choices", [])
    if not choices:
        print(body)
        return
    content = choices[0].get("message", {}).get("content", "")
    try:
        print(json.dumps(json.loads(content), indent=2))
    except json.JSONDecodeError:
        print(content)


if __name__ == "__main__":
    main()
