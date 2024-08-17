import argparse
import json
import os
import subprocess
import tempfile
from typing import Dict, List, Optional

from dotenv import load_dotenv

# Load environment variables
load_dotenv()

URL = "https://generativelanguage.googleapis.com/v1beta/models/{model}:generateContent?key={key}"


def effify(non_f_str: str, **kwargs):
    return eval(f'f"""{non_f_str}"""', kwargs)


def remove_attribute(obj, attr):
    if isinstance(obj, dict):
        if attr in obj:
            del obj[attr]
        for key, value in obj.items():
            remove_attribute(value, attr)
    elif isinstance(obj, list):
        for item in obj:
            remove_attribute(item, attr)
    return obj


def read_file(file_path: str) -> str:
    """Read the content of the file."""
    try:
        with open(file_path, "r") as file:
            return file.read()
    except IOError as e:
        raise IOError(f"Error reading file {file_path}: {e}")


def prepare_context_prompt(context_files: List[str]) -> str:
    """Prepare the full system prompt including context files."""
    context_prompt = ""
    for context_file in context_files:
        file_name = os.path.basename(context_file)
        try:
            file_content = read_file(context_file)
            context_prompt += f"\n\nRequest context in {file_name}:\n{file_content}"
        except IOError as e:
            print(f"Warning: {e}")
    return context_prompt


def execute_curl_command(curl_command: List[str]) -> str:
    """Execute the curl command and return the result."""
    try:
        result = subprocess.run(
            curl_command, capture_output=True, text=True, check=True
        )
        return result.stdout
    except subprocess.CalledProcessError as e:
        raise RuntimeError(f"Error executing curl command: {e.stderr}")


def get_gemini_response(
    api_key: str,
    model: str,
    temperature: float,
    system_prompt: str,
    user_prompt: str,
    context_files: List[str],
    response_format: Optional[Dict],
) -> str:
    """Generate response using Gemini API via curl."""
    context_prompt = prepare_context_prompt(context_files)

    payload = {
        "system_instruction": {"parts": [{"text": system_prompt}]},
        "contents": [
            {
                "role": "user",
                "parts": [{"text": context_prompt}, {"text": user_prompt}],
            }
        ],
        "generationConfig": {
            "temperature": temperature,
        },
    }
    if response_format:
        payload["generationConfig"].update(
            {
                "response_mime_type": "application/json",
                "response_schema": remove_attribute(
                    response_format, "additionalProperties"
                ),
            }
        )
    with tempfile.NamedTemporaryFile(mode="w+", delete=False) as temp_file:
        json.dump(payload, temp_file)
        temp_file_path = temp_file.name

    curl_command = [
        "curl",
        "-X",
        "POST",
        f"{effify(URL, model=model, key=api_key)}",
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
        description="Generate reviews using LLM with Gemini API."
    )
    parser.add_argument(
        "--api_key", default=os.getenv("GEMINI_API_KEY"), help="API key"
    )
    parser.add_argument(
        "--model", default="gemini-1.5-flash", help="Model to use to use"
    )
    parser.add_argument(
        "--temperature",
        type=float,
        default=0.6,
        help="Temperature for sampling (default: 0.6 - good for creative writing)",
    )
    parser.add_argument(
        "--system_prompt",
        default="You are a large language model and a writing assistant. Respond concisely.",
        help="System prompt",
    )
    parser.add_argument(
        "--context_files", nargs="*", default=[], help="Paths to context files"
    )
    parser.add_argument("--user_prompt", required=True, help="User prompt")
    parser.add_argument("--response_format", help="JSON output format file")
    return parser.parse_args()


def main():
    args = parse_arguments()

    response_format = None
    if args.response_format:
        try:
            with open(args.response_format, "r") as schema_file:
                response_format = json.load(schema_file)
        except (IOError, json.JSONDecodeError) as e:
            print(f"Error reading response format file: {e}")
            return

    try:
        response = get_gemini_response(
            args.api_key,
            args.model,
            args.temperature,
            args.system_prompt,
            args.user_prompt,
            args.context_files,
            response_format,
        )
        response_json = json.loads(response)
        candidates = response_json.get("candidates", [])
        if len(candidates) == 0:
            print(response)
        elif len(candidates) == 1:
            message_content = (
                candidates[0].get("content", {}).get("parts", [{}])[0].get("text", "")
            )
            try:
                content = json.loads(message_content)
                print(json.dumps(content, indent=2))
            except json.JSONDecodeError:
                print(message_content)
        else:
            for choice in candidates:
                message_content = (
                    choice.get("content", {}).get("parts", [{}])[0].get("text", "")
                )
                try:
                    content = json.loads(message_content)
                    print(
                        f"Message {choice.get('index', 'N/A')}: {json.dumps(content, indent=2)}"
                    )
                except json.JSONDecodeError:
                    print(f"Message {choice.get('index', 'N/A')}: {message_content}")
    except Exception as e:
        print(f"An error occurred: {e}")


if __name__ == "__main__":
    main()
