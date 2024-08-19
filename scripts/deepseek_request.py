#!/usr/bin/env python
import argparse
import json
import os
import subprocess
import tempfile
from typing import Dict, List, Optional

try:
    from dotenv import load_dotenv

    # Load environment variables
    load_dotenv()
except ImportError:
    dotenv = None

URL = "https://api.deepseek.com/v1/chat/completions"


def read_file(file_path: str) -> str:
    """Read the content of the file."""
    try:
        with open(file_path, "r") as file:
            return file.read()
    except IOError as e:
        raise IOError(f"Error reading file {file_path}: {e}")


def prepare_system_prompt(system_prompt: str, context_files: List[str]) -> str:
    """Prepare the full system prompt including context files."""
    full_prompt = system_prompt
    for context_file in context_files:
        file_name = os.path.basename(context_file)
        try:
            file_content = read_file(context_file)
            full_prompt += f"\n\nRequest context in {file_name}:\n{file_content}"
        except IOError as e:
            print(f"Warning: {e}")
    return full_prompt


def execute_curl_command(curl_command: List[str]) -> str:
    """Execute the curl command and return the result."""
    try:
        result = subprocess.run(curl_command,
                                capture_output=True,
                                text=True,
                                check=True)
        return result.stdout
    except subprocess.CalledProcessError as e:
        raise RuntimeError(f"Error executing curl command: {e.stderr}")


def get_deepseek_response(
    api_key: str,
    model: str,
    temperature: float,
    system_prompt: str,
    user_prompt: str,
    context_files: List[str],
    response_format: Optional[Dict],
) -> str:
    """Generate response using Deepseek API via curl."""
    full_system_prompt = prepare_system_prompt(system_prompt, context_files)

    payload = {
        "model": model,
        "temperature": temperature,
    }
    if response_format:
        payload["response_format"] = {"type": "json_object"}
        full_system_prompt += f"\n{response_format}"
    payload["messages"] = [
        {
            "role": "system",
            "content": full_system_prompt
        },
        {
            "role": "user",
            "content": user_prompt
        },
    ]
    with tempfile.NamedTemporaryFile(mode="w+", delete=False) as temp_file:
        json.dump(payload, temp_file)
        temp_file_path = temp_file.name

    curl_command = [
        "curl",
        "-X",
        "POST",
        URL,
        "-H",
        f"Authorization: Bearer {api_key}",
        "-H",
        "Content-Type: application/json",
        "--data-binary",
        f"@{temp_file_path}",
    ]

    try:
        response = execute_curl_command(curl_command)
    finally:
        # Clean up the temporary file
        os.unlink(temp_file_path)

    return response


def parse_arguments() -> argparse.Namespace:
    """Parse command line arguments."""
    parser = argparse.ArgumentParser(
        description="Generate reviews using LLM with Deepseek API.")
    parser.add_argument("--api_key",
                        default=os.getenv("DEEPSEEK_API_KEY"),
                        help="API key")
    parser.add_argument("--model",
                        default="deepseek-chat",
                        help="Model to use to use")
    parser.add_argument(
        "--temperature",
        type=float,
        default=0.6,
        help=
        "Temperature for sampling (default: 0.6 - good for creative writing)",
    )
    parser.add_argument(
        "--system_prompt",
        default=
        "You are a large language model and a writing assistant. Respond concisely.",
        help="System prompt",
    )
    parser.add_argument("--context_files",
                        nargs="*",
                        default=[],
                        help="Paths to context files")
    parser.add_argument("--user_prompt", required=True, help="User prompt")
    parser.add_argument("--response_format", help="JSON output format file")
    return parser.parse_args()


def main():
    args = parse_arguments()

    response_format = None
    if args.response_format:
        response_format = read_file(args.response_format)

    try:
        response = get_deepseek_response(
            args.api_key,
            args.model,
            args.temperature,
            args.system_prompt,
            args.user_prompt,
            args.context_files,
            response_format,
        )
        response_json = json.loads(response)
        choices = response_json.get("choices", [])
        if len(choices) == 0:
            print(response)
        elif len(choices) == 1:
            message_content = choices[0].get("message",
                                             {}).get("content", "{}")
            try:
                content = json.loads(message_content)
                print(json.dumps(content, indent=2))
            except json.JSONDecodeError:
                print(message_content)
        else:
            for choice in choices:
                message_content = choice.get("message",
                                             {}).get("content", "{}")
                try:
                    content = json.loads(message_content)
                    print(
                        f"Message {choice.get('index', 'N/A')}: {json.dumps(content, indent=2)}"
                    )
                except json.JSONDecodeError:
                    print(
                        f"Message {choice.get('index', 'N/A')}: {message_content}"
                    )
    except Exception as e:
        print(f"An error occurred: {e}")


if __name__ == "__main__":
    main()
