import argparse
import json
import os
import subprocess
from typing import List, Dict, Optional
from dotenv import load_dotenv

# Load environment variables
load_dotenv()

URL = "https://api.openai.com/v1/chat/completions"


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
        result = subprocess.run(
            curl_command, capture_output=True, text=True, check=True
        )
        return result.stdout
    except subprocess.CalledProcessError as e:
        raise RuntimeError(f"Error executing curl command: {e.stderr}")


def get_openai_response(
    api_key: str,
    model: str,
    system_prompt: str,
    user_prompt: str,
    context_files: List[str],
    response_format: Optional[Dict],
) -> str:
    """Generate response using OpenAI API via curl."""
    full_system_prompt = prepare_system_prompt(system_prompt, context_files)
    messages = [
        {"role": "system", "content": full_system_prompt},
        {"role": "user", "content": user_prompt},
    ]

    payload = {
        "model": model,
        "messages": messages,
    }
    if response_format:
        payload["response_format"] = {
            "type": "json_schema",
            "json_schema": {
                "name": "json_response",
                "strict": True,
                "schema": response_format
            }
        }

    curl_command = [
        "curl",
        URL,
        "-H",
        f"Authorization: Bearer {api_key}",
        "-H",
        "Content-Type: application/json",
        "-d",
        json.dumps(payload),
    ]

    return execute_curl_command(curl_command)


def parse_arguments() -> argparse.Namespace:
    """Parse command line arguments."""
    parser = argparse.ArgumentParser(
        description="Generate reviews using LLM with OpenAI compatible API."
    )
    parser.add_argument(
        "--api_key", default=os.getenv("OPENAI_API_KEY"), help="API key"
    )
    parser.add_argument("--model", default="gpt-4o-mini", help="Model to use to use")
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
        response = get_openai_response(
            args.api_key,
            args.model,
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
            message_content = choices[0].get("message", {}).get("content", "{}")
            try:
                content = json.loads(message_content)
                print(json.dumps(content, indent=2))
            except json.JSONDecodeError:
                print(message_content)
        else:
            for choice in choices:
                message_content = choice.get("message", {}).get("content", "{}")
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
