complete -c uv -n "__fish_use_subcommand" -l color -d 'Control colors in output' -r -f -a "{auto	'Enables colored output only when the output is going to a terminal or TTY with support',always	'Enables colored output regardless of the detected environment',never	'Disables colored output'}"
complete -c uv -n "__fish_use_subcommand" -l cache-dir -d 'Path to the cache directory' -r -F
complete -c uv -n "__fish_use_subcommand" -l config-file -d 'The path to a `uv.toml` file to use for configuration' -r -F
complete -c uv -n "__fish_use_subcommand" -s q -l quiet -d 'Do not print any output'
complete -c uv -n "__fish_use_subcommand" -s v -l verbose -d 'Use verbose output'
complete -c uv -n "__fish_use_subcommand" -l no-color -d 'Disable colors; provided for compatibility with `pip`'
complete -c uv -n "__fish_use_subcommand" -l native-tls -d 'Whether to load TLS certificates from the platform\'s native certificate store'
complete -c uv -n "__fish_use_subcommand" -l no-native-tls
complete -c uv -n "__fish_use_subcommand" -l offline -d 'Disable network access, relying only on locally cached data and locally available files'
complete -c uv -n "__fish_use_subcommand" -l no-offline
complete -c uv -n "__fish_use_subcommand" -l preview -d 'Whether to enable experimental, preview features'
complete -c uv -n "__fish_use_subcommand" -l no-preview
complete -c uv -n "__fish_use_subcommand" -l isolated -d 'Avoid discovering a `pyproject.toml` or `uv.toml` file in the current directory or any parent directories'
complete -c uv -n "__fish_use_subcommand" -l show-settings -d 'Show the resolved settings for the current command'
complete -c uv -n "__fish_use_subcommand" -s n -l no-cache -d 'Avoid reading from or writing to the cache'
complete -c uv -n "__fish_use_subcommand" -s h -l help -d 'Print help (see more with \'--help\')'
complete -c uv -n "__fish_use_subcommand" -s V -l version -d 'Print version'
complete -c uv -n "__fish_use_subcommand" -f -a "pip" -d 'Resolve and install Python packages'
complete -c uv -n "__fish_use_subcommand" -f -a "tool" -d 'Run and manage executable Python packages'
complete -c uv -n "__fish_use_subcommand" -f -a "toolchain" -d 'Manage Python installations'
complete -c uv -n "__fish_use_subcommand" -f -a "run" -d 'Run a command in the project environment'
complete -c uv -n "__fish_use_subcommand" -f -a "sync" -d 'Sync the project\'s dependencies with the environment'
complete -c uv -n "__fish_use_subcommand" -f -a "lock" -d 'Resolve the project requirements into a lockfile'
complete -c uv -n "__fish_use_subcommand" -f -a "add" -d 'Add one or more packages to the project requirements'
complete -c uv -n "__fish_use_subcommand" -f -a "remove" -d 'Remove one or more packages from the project requirements'
complete -c uv -n "__fish_use_subcommand" -f -a "venv" -d 'Create a virtual environment'
complete -c uv -n "__fish_use_subcommand" -f -a "cache" -d 'Manage the cache'
complete -c uv -n "__fish_use_subcommand" -f -a "self" -d 'Manage the `uv` executable'
complete -c uv -n "__fish_use_subcommand" -f -a "clean" -d 'Clear the cache, removing all entries or those linked to specific packages'
complete -c uv -n "__fish_use_subcommand" -f -a "version" -d 'Display uv\'s version'
complete -c uv -n "__fish_use_subcommand" -f -a "generate-shell-completion" -d 'Generate shell completion'
complete -c uv -n "__fish_use_subcommand" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c uv -n "__fish_seen_subcommand_from pip; and not __fish_seen_subcommand_from compile; and not __fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from uninstall; and not __fish_seen_subcommand_from freeze; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from show; and not __fish_seen_subcommand_from check; and not __fish_seen_subcommand_from help" -l color -d 'Control colors in output' -r -f -a "{auto	'Enables colored output only when the output is going to a terminal or TTY with support',always	'Enables colored output regardless of the detected environment',never	'Disables colored output'}"
complete -c uv -n "__fish_seen_subcommand_from pip; and not __fish_seen_subcommand_from compile; and not __fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from uninstall; and not __fish_seen_subcommand_from freeze; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from show; and not __fish_seen_subcommand_from check; and not __fish_seen_subcommand_from help" -l cache-dir -d 'Path to the cache directory' -r -F
complete -c uv -n "__fish_seen_subcommand_from pip; and not __fish_seen_subcommand_from compile; and not __fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from uninstall; and not __fish_seen_subcommand_from freeze; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from show; and not __fish_seen_subcommand_from check; and not __fish_seen_subcommand_from help" -l config-file -d 'The path to a `uv.toml` file to use for configuration' -r -F
complete -c uv -n "__fish_seen_subcommand_from pip; and not __fish_seen_subcommand_from compile; and not __fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from uninstall; and not __fish_seen_subcommand_from freeze; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from show; and not __fish_seen_subcommand_from check; and not __fish_seen_subcommand_from help" -s q -l quiet -d 'Do not print any output'
complete -c uv -n "__fish_seen_subcommand_from pip; and not __fish_seen_subcommand_from compile; and not __fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from uninstall; and not __fish_seen_subcommand_from freeze; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from show; and not __fish_seen_subcommand_from check; and not __fish_seen_subcommand_from help" -s v -l verbose -d 'Use verbose output'
complete -c uv -n "__fish_seen_subcommand_from pip; and not __fish_seen_subcommand_from compile; and not __fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from uninstall; and not __fish_seen_subcommand_from freeze; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from show; and not __fish_seen_subcommand_from check; and not __fish_seen_subcommand_from help" -l no-color -d 'Disable colors; provided for compatibility with `pip`'
complete -c uv -n "__fish_seen_subcommand_from pip; and not __fish_seen_subcommand_from compile; and not __fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from uninstall; and not __fish_seen_subcommand_from freeze; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from show; and not __fish_seen_subcommand_from check; and not __fish_seen_subcommand_from help" -l native-tls -d 'Whether to load TLS certificates from the platform\'s native certificate store'
complete -c uv -n "__fish_seen_subcommand_from pip; and not __fish_seen_subcommand_from compile; and not __fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from uninstall; and not __fish_seen_subcommand_from freeze; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from show; and not __fish_seen_subcommand_from check; and not __fish_seen_subcommand_from help" -l no-native-tls
complete -c uv -n "__fish_seen_subcommand_from pip; and not __fish_seen_subcommand_from compile; and not __fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from uninstall; and not __fish_seen_subcommand_from freeze; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from show; and not __fish_seen_subcommand_from check; and not __fish_seen_subcommand_from help" -l offline -d 'Disable network access, relying only on locally cached data and locally available files'
complete -c uv -n "__fish_seen_subcommand_from pip; and not __fish_seen_subcommand_from compile; and not __fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from uninstall; and not __fish_seen_subcommand_from freeze; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from show; and not __fish_seen_subcommand_from check; and not __fish_seen_subcommand_from help" -l no-offline
complete -c uv -n "__fish_seen_subcommand_from pip; and not __fish_seen_subcommand_from compile; and not __fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from uninstall; and not __fish_seen_subcommand_from freeze; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from show; and not __fish_seen_subcommand_from check; and not __fish_seen_subcommand_from help" -l preview -d 'Whether to enable experimental, preview features'
complete -c uv -n "__fish_seen_subcommand_from pip; and not __fish_seen_subcommand_from compile; and not __fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from uninstall; and not __fish_seen_subcommand_from freeze; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from show; and not __fish_seen_subcommand_from check; and not __fish_seen_subcommand_from help" -l no-preview
complete -c uv -n "__fish_seen_subcommand_from pip; and not __fish_seen_subcommand_from compile; and not __fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from uninstall; and not __fish_seen_subcommand_from freeze; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from show; and not __fish_seen_subcommand_from check; and not __fish_seen_subcommand_from help" -l isolated -d 'Avoid discovering a `pyproject.toml` or `uv.toml` file in the current directory or any parent directories'
complete -c uv -n "__fish_seen_subcommand_from pip; and not __fish_seen_subcommand_from compile; and not __fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from uninstall; and not __fish_seen_subcommand_from freeze; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from show; and not __fish_seen_subcommand_from check; and not __fish_seen_subcommand_from help" -l show-settings -d 'Show the resolved settings for the current command'
complete -c uv -n "__fish_seen_subcommand_from pip; and not __fish_seen_subcommand_from compile; and not __fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from uninstall; and not __fish_seen_subcommand_from freeze; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from show; and not __fish_seen_subcommand_from check; and not __fish_seen_subcommand_from help" -s n -l no-cache -d 'Avoid reading from or writing to the cache'
complete -c uv -n "__fish_seen_subcommand_from pip; and not __fish_seen_subcommand_from compile; and not __fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from uninstall; and not __fish_seen_subcommand_from freeze; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from show; and not __fish_seen_subcommand_from check; and not __fish_seen_subcommand_from help" -s h -l help -d 'Print help (see more with \'--help\')'
complete -c uv -n "__fish_seen_subcommand_from pip; and not __fish_seen_subcommand_from compile; and not __fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from uninstall; and not __fish_seen_subcommand_from freeze; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from show; and not __fish_seen_subcommand_from check; and not __fish_seen_subcommand_from help" -s V -l version -d 'Print version'
complete -c uv -n "__fish_seen_subcommand_from pip; and not __fish_seen_subcommand_from compile; and not __fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from uninstall; and not __fish_seen_subcommand_from freeze; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from show; and not __fish_seen_subcommand_from check; and not __fish_seen_subcommand_from help" -f -a "compile" -d 'Compile a `requirements.in` file to a `requirements.txt` file'
complete -c uv -n "__fish_seen_subcommand_from pip; and not __fish_seen_subcommand_from compile; and not __fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from uninstall; and not __fish_seen_subcommand_from freeze; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from show; and not __fish_seen_subcommand_from check; and not __fish_seen_subcommand_from help" -f -a "sync" -d 'Sync dependencies from a `requirements.txt` file'
complete -c uv -n "__fish_seen_subcommand_from pip; and not __fish_seen_subcommand_from compile; and not __fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from uninstall; and not __fish_seen_subcommand_from freeze; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from show; and not __fish_seen_subcommand_from check; and not __fish_seen_subcommand_from help" -f -a "install" -d 'Install packages into the current environment'
complete -c uv -n "__fish_seen_subcommand_from pip; and not __fish_seen_subcommand_from compile; and not __fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from uninstall; and not __fish_seen_subcommand_from freeze; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from show; and not __fish_seen_subcommand_from check; and not __fish_seen_subcommand_from help" -f -a "uninstall" -d 'Uninstall packages from the current environment'
complete -c uv -n "__fish_seen_subcommand_from pip; and not __fish_seen_subcommand_from compile; and not __fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from uninstall; and not __fish_seen_subcommand_from freeze; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from show; and not __fish_seen_subcommand_from check; and not __fish_seen_subcommand_from help" -f -a "freeze" -d 'Enumerate the installed packages in the current environment'
complete -c uv -n "__fish_seen_subcommand_from pip; and not __fish_seen_subcommand_from compile; and not __fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from uninstall; and not __fish_seen_subcommand_from freeze; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from show; and not __fish_seen_subcommand_from check; and not __fish_seen_subcommand_from help" -f -a "list" -d 'Enumerate the installed packages in the current environment'
complete -c uv -n "__fish_seen_subcommand_from pip; and not __fish_seen_subcommand_from compile; and not __fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from uninstall; and not __fish_seen_subcommand_from freeze; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from show; and not __fish_seen_subcommand_from check; and not __fish_seen_subcommand_from help" -f -a "show" -d 'Show information about one or more installed packages'
complete -c uv -n "__fish_seen_subcommand_from pip; and not __fish_seen_subcommand_from compile; and not __fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from uninstall; and not __fish_seen_subcommand_from freeze; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from show; and not __fish_seen_subcommand_from check; and not __fish_seen_subcommand_from help" -f -a "check" -d 'Verify installed packages have compatible dependencies'
complete -c uv -n "__fish_seen_subcommand_from pip; and not __fish_seen_subcommand_from compile; and not __fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from uninstall; and not __fish_seen_subcommand_from freeze; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from show; and not __fish_seen_subcommand_from check; and not __fish_seen_subcommand_from help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -s c -l constraint -d 'Constrain versions using the given requirements files' -r
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l override -d 'Override versions using the given requirements files' -r -F
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l extra -d 'Include optional dependencies from the extra group name; may be provided more than once. Only applies to `pyproject.toml`, `setup.py`, and `setup.cfg` sources' -r
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -s i -l index-url -d 'The URL of the Python package index (by default: <https://pypi.org/simple>)' -r
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l extra-index-url -d 'Extra URLs of package indexes to use, in addition to `--index-url`' -r
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -s f -l find-links -d 'Locations to search for candidate distributions, beyond those found in the indexes' -r
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -s P -l upgrade-package -d 'Allow upgrades for a specific package, ignoring pinned versions in any existing output file' -r
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l index-strategy -d 'The strategy to use when resolving against multiple index URLs' -r -f -a "{first-index	'Only use results from the first index that returns a match for a given package name',unsafe-first-match	'Search for every package name across all indexes, exhausting the versions from the first index before moving on to the next',unsafe-best-match	'Search for every package name across all indexes, preferring the "best" version found. If a package version is in multiple indexes, only look at the entry for the first index'}"
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l keyring-provider -d 'Attempt to use `keyring` for authentication for index URLs' -r -f -a "{disabled	'Do not use keyring for credential lookup',subprocess	'Use the `keyring` command for credential lookup'}"
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l resolution -d 'The strategy to use when selecting between the different compatible versions for a given package requirement' -r -f -a "{highest	'Resolve the highest compatible version of each package',lowest	'Resolve the lowest compatible version of each package',lowest-direct	'Resolve the lowest compatible version of any direct dependencies, and the highest compatible version of any transitive dependencies'}"
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l prerelease -d 'The strategy to use when considering pre-release versions' -r -f -a "{disallow	'Disallow all pre-release versions',allow	'Allow all pre-release versions',if-necessary	'Allow pre-release versions if all versions of a package are pre-release',explicit	'Allow pre-release versions for first-party packages with explicit pre-release markers in their version requirements',if-necessary-or-explicit	'Allow pre-release versions if all versions of a package are pre-release, or if the package has an explicit pre-release marker in its version requirements'}"
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -s C -l config-setting -d 'Settings to pass to the PEP 517 build backend, specified as `KEY=VALUE` pairs' -r
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l exclude-newer -d 'Limit candidate packages to those that were uploaded prior to the given date' -r
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l link-mode -d 'The method to use when installing packages from the global cache' -r -f -a "{clone	'Clone (i.e., copy-on-write) packages from the wheel into the site packages',copy	'Copy packages from the wheel into the site packages',hardlink	'Hard link packages from the wheel into the site packages'}"
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l refresh-package -d 'Refresh cached data for a specific package' -r
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -s o -l output-file -d 'Write the compiled requirements to the given `requirements.txt` file' -r -F
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l annotation-style -d 'Choose the style of the annotation comments, which indicate the source of each package' -r -f -a "{line	'Render the annotations on a single, comma-separated line',split	'Render each annotation on its own line'}"
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l custom-compile-command -d 'Change header comment to reflect custom command wrapping `uv pip compile`' -r
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l python -d 'The Python interpreter against which to compile the requirements.' -r
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l no-binary -d 'Don\'t install pre-built wheels' -r
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l only-binary -d 'Only use pre-built wheels; don\'t build source distributions' -r
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -s p -l python-version -d 'The minimum Python version that should be supported by the compiled requirements (e.g., `3.7` or `3.7.9`)' -r
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l python-platform -d 'The platform for which requirements should be resolved' -r -f -a "{windows	'An alias for `x86_64-pc-windows-msvc`, the default target for Windows',linux	'An alias for `x86_64-unknown-linux-gnu`, the default target for Linux',macos	'An alias for `aarch64-apple-darwin`, the default target for macOS',x86_64-pc-windows-msvc	'An x86 Windows target',x86_64-unknown-linux-gnu	'An x86 Linux target. Equivalent to `x86_64-manylinux_2_17`',aarch64-apple-darwin	'An ARM-based macOS target, as seen on Apple Silicon devices',x86_64-apple-darwin	'An x86 macOS target',aarch64-unknown-linux-gnu	'An ARM64 Linux target. Equivalent to `aarch64-manylinux_2_17`',aarch64-unknown-linux-musl	'An ARM64 Linux target',x86_64-unknown-linux-musl	'An `x86_64` Linux target',x86_64-manylinux_2_17	'An `x86_64` target for the `manylinux_2_17` platform',x86_64-manylinux_2_28	'An `x86_64` target for the `manylinux_2_28` platform',aarch64-manylinux_2_17	'An ARM64 target for the `manylinux_2_17` platform',aarch64-manylinux_2_28	'An ARM64 target for the `manylinux_2_28` platform'}"
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l no-emit-package -d 'Specify a package to omit from the output resolution. Its dependencies will still be included in the resolution. Equivalent to pip-compile\'s `--unsafe-package` option' -r
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l resolver -r -f -a "{backtracking	'',legacy	''}"
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l max-rounds -r
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l cert -r
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l client-cert -r
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l trusted-host -r
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l config -r
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l pip-args -r
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l color -d 'Control colors in output' -r -f -a "{auto	'Enables colored output only when the output is going to a terminal or TTY with support',always	'Enables colored output regardless of the detected environment',never	'Disables colored output'}"
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l cache-dir -d 'Path to the cache directory' -r -F
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l config-file -d 'The path to a `uv.toml` file to use for configuration' -r -F
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l all-extras -d 'Include all optional dependencies. Only applies to `pyproject.toml`, `setup.py`, and `setup.cfg` sources'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l no-all-extras
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l no-index -d 'Ignore the registry index (e.g., PyPI), instead relying on direct URL dependencies and those discovered via `--find-links`'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -s U -l upgrade -d 'Allow package upgrades, ignoring pinned versions in any existing output file'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l no-upgrade
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l pre
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l refresh -d 'Refresh all cached data'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l no-refresh
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l no-deps -d 'Ignore package dependencies, instead only add those packages explicitly listed on the command line to the resulting the requirements file'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l deps
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l no-strip-extras -d 'Include extras in the output file'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l strip-extras
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l no-annotate -d 'Exclude comment annotations indicating the source of each package'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l annotate
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l no-header -d 'Exclude the comment header at the top of the generated output file'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l header
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l system -d 'Install packages into the system Python'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l no-system
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l generate-hashes -d 'Include distribution hashes in the output file'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l no-generate-hashes
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l legacy-setup-py -d 'Use legacy `setuptools` behavior when building source distributions without a `pyproject.toml`'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l no-legacy-setup-py
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l no-build-isolation -d 'Disable isolation when building source distributions'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l build-isolation
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l no-build -d 'Don\'t build source distributions'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l build
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l emit-index-url -d 'Include `--index-url` and `--extra-index-url` entries in the generated output file'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l no-emit-index-url
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l emit-find-links -d 'Include `--find-links` entries in the generated output file'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l no-emit-find-links
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l emit-marker-expression -d 'Whether to emit a marker string indicating when it is known that the resulting set of pinned dependencies is valid'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l no-emit-marker-expression
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l emit-index-annotation -d 'Include comment annotations indicating the index used to resolve each package (e.g., `# from https://pypi.org/simple`)'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l no-emit-index-annotation
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l allow-unsafe
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l no-allow-unsafe
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l reuse-hashes
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l no-reuse-hashes
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l emit-trusted-host
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l no-emit-trusted-host
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l no-config
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l emit-options
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l no-emit-options
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -s q -l quiet -d 'Do not print any output'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -s v -l verbose -d 'Use verbose output'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l no-color -d 'Disable colors; provided for compatibility with `pip`'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l native-tls -d 'Whether to load TLS certificates from the platform\'s native certificate store'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l no-native-tls
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l offline -d 'Disable network access, relying only on locally cached data and locally available files'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l no-offline
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l preview -d 'Whether to enable experimental, preview features'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l no-preview
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l isolated -d 'Avoid discovering a `pyproject.toml` or `uv.toml` file in the current directory or any parent directories'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -l show-settings -d 'Show the resolved settings for the current command'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -s n -l no-cache -d 'Avoid reading from or writing to the cache'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -s h -l help -d 'Print help (see more with \'--help\')'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from compile" -s V -l version -d 'Print version'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -s c -l constraint -d 'Constrain versions using the given requirements files' -r
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -s i -l index-url -d 'The URL of the Python package index (by default: <https://pypi.org/simple>)' -r
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -l extra-index-url -d 'Extra URLs of package indexes to use, in addition to `--index-url`' -r
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -s f -l find-links -d 'Locations to search for candidate distributions, beyond those found in the indexes' -r
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -l reinstall-package -d 'Reinstall a specific package, regardless of whether it\'s already installed' -r
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -l index-strategy -d 'The strategy to use when resolving against multiple index URLs' -r -f -a "{first-index	'Only use results from the first index that returns a match for a given package name',unsafe-first-match	'Search for every package name across all indexes, exhausting the versions from the first index before moving on to the next',unsafe-best-match	'Search for every package name across all indexes, preferring the "best" version found. If a package version is in multiple indexes, only look at the entry for the first index'}"
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -l keyring-provider -d 'Attempt to use `keyring` for authentication for index URLs' -r -f -a "{disabled	'Do not use keyring for credential lookup',subprocess	'Use the `keyring` command for credential lookup'}"
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -s C -l config-setting -d 'Settings to pass to the PEP 517 build backend, specified as `KEY=VALUE` pairs' -r
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -l link-mode -d 'The method to use when installing packages from the global cache' -r -f -a "{clone	'Clone (i.e., copy-on-write) packages from the wheel into the site packages',copy	'Copy packages from the wheel into the site packages',hardlink	'Hard link packages from the wheel into the site packages'}"
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -l refresh-package -d 'Refresh cached data for a specific package' -r
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -l exclude-newer -d 'Limit candidate packages to those that were uploaded prior to the given date' -r
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -s p -l python -d 'The Python interpreter into which packages should be installed.' -r
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -l target -d 'Install packages into the specified directory, rather than into the virtual environment or system Python interpreter. The packages will be installed at the top-level of the directory' -r -F
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -l prefix -d 'Install packages into `lib`, `bin`, and other top-level folders under the specified directory, as if a virtual environment were created at the specified location' -r -F
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -l no-binary -d 'Don\'t install pre-built wheels' -r
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -l only-binary -d 'Only use pre-built wheels; don\'t build source distributions' -r
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -l python-version -d 'The minimum Python version that should be supported by the requirements (e.g., `3.7` or `3.7.9`)' -r
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -l python-platform -d 'The platform for which requirements should be installed' -r -f -a "{windows	'An alias for `x86_64-pc-windows-msvc`, the default target for Windows',linux	'An alias for `x86_64-unknown-linux-gnu`, the default target for Linux',macos	'An alias for `aarch64-apple-darwin`, the default target for macOS',x86_64-pc-windows-msvc	'An x86 Windows target',x86_64-unknown-linux-gnu	'An x86 Linux target. Equivalent to `x86_64-manylinux_2_17`',aarch64-apple-darwin	'An ARM-based macOS target, as seen on Apple Silicon devices',x86_64-apple-darwin	'An x86 macOS target',aarch64-unknown-linux-gnu	'An ARM64 Linux target. Equivalent to `aarch64-manylinux_2_17`',aarch64-unknown-linux-musl	'An ARM64 Linux target',x86_64-unknown-linux-musl	'An `x86_64` Linux target',x86_64-manylinux_2_17	'An `x86_64` target for the `manylinux_2_17` platform',x86_64-manylinux_2_28	'An `x86_64` target for the `manylinux_2_28` platform',aarch64-manylinux_2_17	'An ARM64 target for the `manylinux_2_17` platform',aarch64-manylinux_2_28	'An ARM64 target for the `manylinux_2_28` platform'}"
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -l trusted-host -r
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -l python-executable -r
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -l cert -r
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -l client-cert -r
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -l config -r
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -l pip-args -r
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -l color -d 'Control colors in output' -r -f -a "{auto	'Enables colored output only when the output is going to a terminal or TTY with support',always	'Enables colored output regardless of the detected environment',never	'Disables colored output'}"
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -l cache-dir -d 'Path to the cache directory' -r -F
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -l config-file -d 'The path to a `uv.toml` file to use for configuration' -r -F
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -l no-index -d 'Ignore the registry index (e.g., PyPI), instead relying on direct URL dependencies and those discovered via `--find-links`'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -l reinstall -d 'Reinstall all packages, regardless of whether they\'re already installed'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -l no-reinstall
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -l compile-bytecode -d 'Compile Python files to bytecode'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -l no-compile-bytecode
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -l refresh -d 'Refresh all cached data'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -l no-refresh
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -l require-hashes -d 'Require a matching hash for each requirement'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -l no-require-hashes
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -l system -d 'Install packages into the system Python'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -l no-system
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -l break-system-packages -d 'Allow `uv` to modify an `EXTERNALLY-MANAGED` Python installation'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -l no-break-system-packages
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -l legacy-setup-py -d 'Use legacy `setuptools` behavior when building source distributions without a `pyproject.toml`'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -l no-legacy-setup-py
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -l no-build-isolation -d 'Disable isolation when building source distributions'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -l build-isolation
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -l no-build -d 'Don\'t build source distributions'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -l build
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -l strict -d 'Validate the virtual environment after completing the installation, to detect packages with missing dependencies or other issues'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -l no-strict
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -l dry-run -d 'Perform a dry run, i.e., don\'t actually install anything but resolve the dependencies and print the resulting plan'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -s a -l ask
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -l user
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -l no-config
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -s q -l quiet -d 'Do not print any output'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -s v -l verbose -d 'Use verbose output'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -l no-color -d 'Disable colors; provided for compatibility with `pip`'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -l native-tls -d 'Whether to load TLS certificates from the platform\'s native certificate store'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -l no-native-tls
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -l offline -d 'Disable network access, relying only on locally cached data and locally available files'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -l no-offline
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -l preview -d 'Whether to enable experimental, preview features'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -l no-preview
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -l isolated -d 'Avoid discovering a `pyproject.toml` or `uv.toml` file in the current directory or any parent directories'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -l show-settings -d 'Show the resolved settings for the current command'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -s n -l no-cache -d 'Avoid reading from or writing to the cache'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -s h -l help -d 'Print help (see more with \'--help\')'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from sync" -s V -l version -d 'Print version'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -s r -l requirement -d 'Install all packages listed in the given `requirements.txt` files' -r -F
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -s e -l editable -d 'Install the editable package based on the provided local file path' -r
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -s c -l constraint -d 'Constrain versions using the given requirements files' -r
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -l override -d 'Override versions using the given requirements files' -r -F
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -l extra -d 'Include optional dependencies from the extra group name; may be provided more than once. Only applies to `pyproject.toml`, `setup.py`, and `setup.cfg` sources' -r
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -s i -l index-url -d 'The URL of the Python package index (by default: <https://pypi.org/simple>)' -r
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -l extra-index-url -d 'Extra URLs of package indexes to use, in addition to `--index-url`' -r
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -s f -l find-links -d 'Locations to search for candidate distributions, beyond those found in the indexes' -r
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -s P -l upgrade-package -d 'Allow upgrades for a specific package, ignoring pinned versions in any existing output file' -r
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -l reinstall-package -d 'Reinstall a specific package, regardless of whether it\'s already installed' -r
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -l index-strategy -d 'The strategy to use when resolving against multiple index URLs' -r -f -a "{first-index	'Only use results from the first index that returns a match for a given package name',unsafe-first-match	'Search for every package name across all indexes, exhausting the versions from the first index before moving on to the next',unsafe-best-match	'Search for every package name across all indexes, preferring the "best" version found. If a package version is in multiple indexes, only look at the entry for the first index'}"
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -l keyring-provider -d 'Attempt to use `keyring` for authentication for index URLs' -r -f -a "{disabled	'Do not use keyring for credential lookup',subprocess	'Use the `keyring` command for credential lookup'}"
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -l resolution -d 'The strategy to use when selecting between the different compatible versions for a given package requirement' -r -f -a "{highest	'Resolve the highest compatible version of each package',lowest	'Resolve the lowest compatible version of each package',lowest-direct	'Resolve the lowest compatible version of any direct dependencies, and the highest compatible version of any transitive dependencies'}"
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -l prerelease -d 'The strategy to use when considering pre-release versions' -r -f -a "{disallow	'Disallow all pre-release versions',allow	'Allow all pre-release versions',if-necessary	'Allow pre-release versions if all versions of a package are pre-release',explicit	'Allow pre-release versions for first-party packages with explicit pre-release markers in their version requirements',if-necessary-or-explicit	'Allow pre-release versions if all versions of a package are pre-release, or if the package has an explicit pre-release marker in its version requirements'}"
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -s C -l config-setting -d 'Settings to pass to the PEP 517 build backend, specified as `KEY=VALUE` pairs' -r
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -l exclude-newer -d 'Limit candidate packages to those that were uploaded prior to the given date' -r
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -l link-mode -d 'The method to use when installing packages from the global cache' -r -f -a "{clone	'Clone (i.e., copy-on-write) packages from the wheel into the site packages',copy	'Copy packages from the wheel into the site packages',hardlink	'Hard link packages from the wheel into the site packages'}"
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -l refresh-package -d 'Refresh cached data for a specific package' -r
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -s p -l python -d 'The Python interpreter into which packages should be installed.' -r
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -l target -d 'Install packages into the specified directory, rather than into the virtual environment or system Python interpreter. The packages will be installed at the top-level of the directory' -r -F
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -l prefix -d 'Install packages into `lib`, `bin`, and other top-level folders under the specified directory, as if a virtual environment were created at the specified location' -r -F
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -l no-binary -d 'Don\'t install pre-built wheels' -r
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -l only-binary -d 'Only use pre-built wheels; don\'t build source distributions' -r
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -l python-version -d 'The minimum Python version that should be supported by the requirements (e.g., `3.7` or `3.7.9`)' -r
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -l python-platform -d 'The platform for which requirements should be installed' -r -f -a "{windows	'An alias for `x86_64-pc-windows-msvc`, the default target for Windows',linux	'An alias for `x86_64-unknown-linux-gnu`, the default target for Linux',macos	'An alias for `aarch64-apple-darwin`, the default target for macOS',x86_64-pc-windows-msvc	'An x86 Windows target',x86_64-unknown-linux-gnu	'An x86 Linux target. Equivalent to `x86_64-manylinux_2_17`',aarch64-apple-darwin	'An ARM-based macOS target, as seen on Apple Silicon devices',x86_64-apple-darwin	'An x86 macOS target',aarch64-unknown-linux-gnu	'An ARM64 Linux target. Equivalent to `aarch64-manylinux_2_17`',aarch64-unknown-linux-musl	'An ARM64 Linux target',x86_64-unknown-linux-musl	'An `x86_64` Linux target',x86_64-manylinux_2_17	'An `x86_64` target for the `manylinux_2_17` platform',x86_64-manylinux_2_28	'An `x86_64` target for the `manylinux_2_28` platform',aarch64-manylinux_2_17	'An ARM64 target for the `manylinux_2_17` platform',aarch64-manylinux_2_28	'An ARM64 target for the `manylinux_2_28` platform'}"
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -l color -d 'Control colors in output' -r -f -a "{auto	'Enables colored output only when the output is going to a terminal or TTY with support',always	'Enables colored output regardless of the detected environment',never	'Disables colored output'}"
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -l cache-dir -d 'Path to the cache directory' -r -F
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -l config-file -d 'The path to a `uv.toml` file to use for configuration' -r -F
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -l all-extras -d 'Include all optional dependencies. Only applies to `pyproject.toml`, `setup.py`, and `setup.cfg` sources'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -l no-all-extras
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -l no-index -d 'Ignore the registry index (e.g., PyPI), instead relying on direct URL dependencies and those discovered via `--find-links`'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -s U -l upgrade -d 'Allow package upgrades, ignoring pinned versions in any existing output file'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -l no-upgrade
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -l reinstall -d 'Reinstall all packages, regardless of whether they\'re already installed'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -l no-reinstall
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -l pre
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -l compile-bytecode -d 'Compile Python files to bytecode'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -l no-compile-bytecode
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -l refresh -d 'Refresh all cached data'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -l no-refresh
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -l no-deps -d 'Ignore package dependencies, instead only installing those packages explicitly listed on the command line or in the requirements files'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -l deps
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -l require-hashes -d 'Require a matching hash for each requirement'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -l no-require-hashes
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -l system -d 'Install packages into the system Python'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -l no-system
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -l break-system-packages -d 'Allow `uv` to modify an `EXTERNALLY-MANAGED` Python installation'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -l no-break-system-packages
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -l legacy-setup-py -d 'Use legacy `setuptools` behavior when building source distributions without a `pyproject.toml`'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -l no-legacy-setup-py
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -l no-build-isolation -d 'Disable isolation when building source distributions'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -l build-isolation
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -l no-build -d 'Don\'t build source distributions'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -l build
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -l strict -d 'Validate the virtual environment after completing the installation, to detect packages with missing dependencies or other issues'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -l no-strict
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -l dry-run -d 'Perform a dry run, i.e., don\'t actually install anything but resolve the dependencies and print the resulting plan'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -l user
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -s q -l quiet -d 'Do not print any output'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -s v -l verbose -d 'Use verbose output'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -l no-color -d 'Disable colors; provided for compatibility with `pip`'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -l native-tls -d 'Whether to load TLS certificates from the platform\'s native certificate store'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -l no-native-tls
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -l offline -d 'Disable network access, relying only on locally cached data and locally available files'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -l no-offline
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -l preview -d 'Whether to enable experimental, preview features'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -l no-preview
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -l isolated -d 'Avoid discovering a `pyproject.toml` or `uv.toml` file in the current directory or any parent directories'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -l show-settings -d 'Show the resolved settings for the current command'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -s n -l no-cache -d 'Avoid reading from or writing to the cache'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -s h -l help -d 'Print help (see more with \'--help\')'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from install" -s V -l version -d 'Print version'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from uninstall" -s r -l requirement -d 'Uninstall all packages listed in the given requirements files' -r -F
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from uninstall" -s p -l python -d 'The Python interpreter from which packages should be uninstalled.' -r
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from uninstall" -l keyring-provider -d 'Attempt to use `keyring` for authentication for remote requirements files' -r -f -a "{disabled	'Do not use keyring for credential lookup',subprocess	'Use the `keyring` command for credential lookup'}"
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from uninstall" -l target -d 'Uninstall packages from the specified `--target` directory' -r -F
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from uninstall" -l prefix -d 'Uninstall packages from the specified `--prefix` directory' -r -F
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from uninstall" -l color -d 'Control colors in output' -r -f -a "{auto	'Enables colored output only when the output is going to a terminal or TTY with support',always	'Enables colored output regardless of the detected environment',never	'Disables colored output'}"
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from uninstall" -l cache-dir -d 'Path to the cache directory' -r -F
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from uninstall" -l config-file -d 'The path to a `uv.toml` file to use for configuration' -r -F
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from uninstall" -l system -d 'Use the system Python to uninstall packages'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from uninstall" -l no-system
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from uninstall" -l break-system-packages -d 'Allow `uv` to modify an `EXTERNALLY-MANAGED` Python installation'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from uninstall" -l no-break-system-packages
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from uninstall" -s q -l quiet -d 'Do not print any output'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from uninstall" -s v -l verbose -d 'Use verbose output'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from uninstall" -l no-color -d 'Disable colors; provided for compatibility with `pip`'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from uninstall" -l native-tls -d 'Whether to load TLS certificates from the platform\'s native certificate store'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from uninstall" -l no-native-tls
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from uninstall" -l offline -d 'Disable network access, relying only on locally cached data and locally available files'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from uninstall" -l no-offline
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from uninstall" -l preview -d 'Whether to enable experimental, preview features'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from uninstall" -l no-preview
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from uninstall" -l isolated -d 'Avoid discovering a `pyproject.toml` or `uv.toml` file in the current directory or any parent directories'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from uninstall" -l show-settings -d 'Show the resolved settings for the current command'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from uninstall" -s n -l no-cache -d 'Avoid reading from or writing to the cache'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from uninstall" -s h -l help -d 'Print help (see more with \'--help\')'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from uninstall" -s V -l version -d 'Print version'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from freeze" -s p -l python -d 'The Python interpreter for which packages should be listed.' -r
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from freeze" -l color -d 'Control colors in output' -r -f -a "{auto	'Enables colored output only when the output is going to a terminal or TTY with support',always	'Enables colored output regardless of the detected environment',never	'Disables colored output'}"
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from freeze" -l cache-dir -d 'Path to the cache directory' -r -F
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from freeze" -l config-file -d 'The path to a `uv.toml` file to use for configuration' -r -F
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from freeze" -l exclude-editable -d 'Exclude any editable packages from output'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from freeze" -l strict -d 'Validate the virtual environment, to detect packages with missing dependencies or other issues'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from freeze" -l no-strict
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from freeze" -l system -d 'List packages for the system Python'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from freeze" -l no-system
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from freeze" -s q -l quiet -d 'Do not print any output'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from freeze" -s v -l verbose -d 'Use verbose output'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from freeze" -l no-color -d 'Disable colors; provided for compatibility with `pip`'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from freeze" -l native-tls -d 'Whether to load TLS certificates from the platform\'s native certificate store'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from freeze" -l no-native-tls
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from freeze" -l offline -d 'Disable network access, relying only on locally cached data and locally available files'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from freeze" -l no-offline
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from freeze" -l preview -d 'Whether to enable experimental, preview features'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from freeze" -l no-preview
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from freeze" -l isolated -d 'Avoid discovering a `pyproject.toml` or `uv.toml` file in the current directory or any parent directories'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from freeze" -l show-settings -d 'Show the resolved settings for the current command'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from freeze" -s n -l no-cache -d 'Avoid reading from or writing to the cache'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from freeze" -s h -l help -d 'Print help (see more with \'--help\')'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from freeze" -s V -l version -d 'Print version'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from list" -l exclude -d 'Exclude the specified package(s) from the output' -r
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from list" -l format -d 'Select the output format between: `columns` (default), `freeze`, or `json`' -r -f -a "{columns	'Display the list of packages in a human-readable table',freeze	'Display the list of packages in a `pip freeze`-like format, with one package per line alongside its version',json	'Display the list of packages in a machine-readable JSON format'}"
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from list" -s p -l python -d 'The Python interpreter for which packages should be listed.' -r
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from list" -l color -d 'Control colors in output' -r -f -a "{auto	'Enables colored output only when the output is going to a terminal or TTY with support',always	'Enables colored output regardless of the detected environment',never	'Disables colored output'}"
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from list" -l cache-dir -d 'Path to the cache directory' -r -F
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from list" -l config-file -d 'The path to a `uv.toml` file to use for configuration' -r -F
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from list" -s e -l editable -d 'Only include editable projects'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from list" -l exclude-editable -d 'Exclude any editable packages from output'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from list" -l strict -d 'Validate the virtual environment, to detect packages with missing dependencies or other issues'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from list" -l no-strict
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from list" -l system -d 'List packages for the system Python'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from list" -l no-system
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from list" -l outdated
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from list" -s q -l quiet -d 'Do not print any output'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from list" -s v -l verbose -d 'Use verbose output'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from list" -l no-color -d 'Disable colors; provided for compatibility with `pip`'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from list" -l native-tls -d 'Whether to load TLS certificates from the platform\'s native certificate store'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from list" -l no-native-tls
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from list" -l offline -d 'Disable network access, relying only on locally cached data and locally available files'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from list" -l no-offline
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from list" -l preview -d 'Whether to enable experimental, preview features'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from list" -l no-preview
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from list" -l isolated -d 'Avoid discovering a `pyproject.toml` or `uv.toml` file in the current directory or any parent directories'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from list" -l show-settings -d 'Show the resolved settings for the current command'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from list" -s n -l no-cache -d 'Avoid reading from or writing to the cache'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from list" -s h -l help -d 'Print help (see more with \'--help\')'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from list" -s V -l version -d 'Print version'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from show" -s p -l python -d 'The Python interpreter for which packages should be listed.' -r
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from show" -l color -d 'Control colors in output' -r -f -a "{auto	'Enables colored output only when the output is going to a terminal or TTY with support',always	'Enables colored output regardless of the detected environment',never	'Disables colored output'}"
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from show" -l cache-dir -d 'Path to the cache directory' -r -F
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from show" -l config-file -d 'The path to a `uv.toml` file to use for configuration' -r -F
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from show" -l strict -d 'Validate the virtual environment, to detect packages with missing dependencies or other issues'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from show" -l no-strict
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from show" -l system -d 'List packages for the system Python'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from show" -l no-system
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from show" -s q -l quiet -d 'Do not print any output'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from show" -s v -l verbose -d 'Use verbose output'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from show" -l no-color -d 'Disable colors; provided for compatibility with `pip`'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from show" -l native-tls -d 'Whether to load TLS certificates from the platform\'s native certificate store'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from show" -l no-native-tls
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from show" -l offline -d 'Disable network access, relying only on locally cached data and locally available files'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from show" -l no-offline
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from show" -l preview -d 'Whether to enable experimental, preview features'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from show" -l no-preview
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from show" -l isolated -d 'Avoid discovering a `pyproject.toml` or `uv.toml` file in the current directory or any parent directories'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from show" -l show-settings -d 'Show the resolved settings for the current command'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from show" -s n -l no-cache -d 'Avoid reading from or writing to the cache'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from show" -s h -l help -d 'Print help (see more with \'--help\')'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from show" -s V -l version -d 'Print version'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from check" -s p -l python -d 'The Python interpreter for which packages should be listed.' -r
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from check" -l color -d 'Control colors in output' -r -f -a "{auto	'Enables colored output only when the output is going to a terminal or TTY with support',always	'Enables colored output regardless of the detected environment',never	'Disables colored output'}"
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from check" -l cache-dir -d 'Path to the cache directory' -r -F
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from check" -l config-file -d 'The path to a `uv.toml` file to use for configuration' -r -F
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from check" -l system -d 'List packages for the system Python'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from check" -l no-system
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from check" -s q -l quiet -d 'Do not print any output'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from check" -s v -l verbose -d 'Use verbose output'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from check" -l no-color -d 'Disable colors; provided for compatibility with `pip`'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from check" -l native-tls -d 'Whether to load TLS certificates from the platform\'s native certificate store'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from check" -l no-native-tls
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from check" -l offline -d 'Disable network access, relying only on locally cached data and locally available files'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from check" -l no-offline
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from check" -l preview -d 'Whether to enable experimental, preview features'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from check" -l no-preview
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from check" -l isolated -d 'Avoid discovering a `pyproject.toml` or `uv.toml` file in the current directory or any parent directories'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from check" -l show-settings -d 'Show the resolved settings for the current command'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from check" -s n -l no-cache -d 'Avoid reading from or writing to the cache'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from check" -s h -l help -d 'Print help (see more with \'--help\')'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from check" -s V -l version -d 'Print version'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from help; and not __fish_seen_subcommand_from compile; and not __fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from uninstall; and not __fish_seen_subcommand_from freeze; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from show; and not __fish_seen_subcommand_from check; and not __fish_seen_subcommand_from help" -f -a "compile" -d 'Compile a `requirements.in` file to a `requirements.txt` file'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from help; and not __fish_seen_subcommand_from compile; and not __fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from uninstall; and not __fish_seen_subcommand_from freeze; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from show; and not __fish_seen_subcommand_from check; and not __fish_seen_subcommand_from help" -f -a "sync" -d 'Sync dependencies from a `requirements.txt` file'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from help; and not __fish_seen_subcommand_from compile; and not __fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from uninstall; and not __fish_seen_subcommand_from freeze; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from show; and not __fish_seen_subcommand_from check; and not __fish_seen_subcommand_from help" -f -a "install" -d 'Install packages into the current environment'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from help; and not __fish_seen_subcommand_from compile; and not __fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from uninstall; and not __fish_seen_subcommand_from freeze; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from show; and not __fish_seen_subcommand_from check; and not __fish_seen_subcommand_from help" -f -a "uninstall" -d 'Uninstall packages from the current environment'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from help; and not __fish_seen_subcommand_from compile; and not __fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from uninstall; and not __fish_seen_subcommand_from freeze; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from show; and not __fish_seen_subcommand_from check; and not __fish_seen_subcommand_from help" -f -a "freeze" -d 'Enumerate the installed packages in the current environment'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from help; and not __fish_seen_subcommand_from compile; and not __fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from uninstall; and not __fish_seen_subcommand_from freeze; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from show; and not __fish_seen_subcommand_from check; and not __fish_seen_subcommand_from help" -f -a "list" -d 'Enumerate the installed packages in the current environment'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from help; and not __fish_seen_subcommand_from compile; and not __fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from uninstall; and not __fish_seen_subcommand_from freeze; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from show; and not __fish_seen_subcommand_from check; and not __fish_seen_subcommand_from help" -f -a "show" -d 'Show information about one or more installed packages'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from help; and not __fish_seen_subcommand_from compile; and not __fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from uninstall; and not __fish_seen_subcommand_from freeze; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from show; and not __fish_seen_subcommand_from check; and not __fish_seen_subcommand_from help" -f -a "check" -d 'Verify installed packages have compatible dependencies'
complete -c uv -n "__fish_seen_subcommand_from pip; and __fish_seen_subcommand_from help; and not __fish_seen_subcommand_from compile; and not __fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from uninstall; and not __fish_seen_subcommand_from freeze; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from show; and not __fish_seen_subcommand_from check; and not __fish_seen_subcommand_from help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c uv -n "__fish_seen_subcommand_from tool; and not __fish_seen_subcommand_from run; and not __fish_seen_subcommand_from help" -l color -d 'Control colors in output' -r -f -a "{auto	'Enables colored output only when the output is going to a terminal or TTY with support',always	'Enables colored output regardless of the detected environment',never	'Disables colored output'}"
complete -c uv -n "__fish_seen_subcommand_from tool; and not __fish_seen_subcommand_from run; and not __fish_seen_subcommand_from help" -l cache-dir -d 'Path to the cache directory' -r -F
complete -c uv -n "__fish_seen_subcommand_from tool; and not __fish_seen_subcommand_from run; and not __fish_seen_subcommand_from help" -l config-file -d 'The path to a `uv.toml` file to use for configuration' -r -F
complete -c uv -n "__fish_seen_subcommand_from tool; and not __fish_seen_subcommand_from run; and not __fish_seen_subcommand_from help" -s q -l quiet -d 'Do not print any output'
complete -c uv -n "__fish_seen_subcommand_from tool; and not __fish_seen_subcommand_from run; and not __fish_seen_subcommand_from help" -s v -l verbose -d 'Use verbose output'
complete -c uv -n "__fish_seen_subcommand_from tool; and not __fish_seen_subcommand_from run; and not __fish_seen_subcommand_from help" -l no-color -d 'Disable colors; provided for compatibility with `pip`'
complete -c uv -n "__fish_seen_subcommand_from tool; and not __fish_seen_subcommand_from run; and not __fish_seen_subcommand_from help" -l native-tls -d 'Whether to load TLS certificates from the platform\'s native certificate store'
complete -c uv -n "__fish_seen_subcommand_from tool; and not __fish_seen_subcommand_from run; and not __fish_seen_subcommand_from help" -l no-native-tls
complete -c uv -n "__fish_seen_subcommand_from tool; and not __fish_seen_subcommand_from run; and not __fish_seen_subcommand_from help" -l offline -d 'Disable network access, relying only on locally cached data and locally available files'
complete -c uv -n "__fish_seen_subcommand_from tool; and not __fish_seen_subcommand_from run; and not __fish_seen_subcommand_from help" -l no-offline
complete -c uv -n "__fish_seen_subcommand_from tool; and not __fish_seen_subcommand_from run; and not __fish_seen_subcommand_from help" -l preview -d 'Whether to enable experimental, preview features'
complete -c uv -n "__fish_seen_subcommand_from tool; and not __fish_seen_subcommand_from run; and not __fish_seen_subcommand_from help" -l no-preview
complete -c uv -n "__fish_seen_subcommand_from tool; and not __fish_seen_subcommand_from run; and not __fish_seen_subcommand_from help" -l isolated -d 'Avoid discovering a `pyproject.toml` or `uv.toml` file in the current directory or any parent directories'
complete -c uv -n "__fish_seen_subcommand_from tool; and not __fish_seen_subcommand_from run; and not __fish_seen_subcommand_from help" -l show-settings -d 'Show the resolved settings for the current command'
complete -c uv -n "__fish_seen_subcommand_from tool; and not __fish_seen_subcommand_from run; and not __fish_seen_subcommand_from help" -s n -l no-cache -d 'Avoid reading from or writing to the cache'
complete -c uv -n "__fish_seen_subcommand_from tool; and not __fish_seen_subcommand_from run; and not __fish_seen_subcommand_from help" -s h -l help -d 'Print help (see more with \'--help\')'
complete -c uv -n "__fish_seen_subcommand_from tool; and not __fish_seen_subcommand_from run; and not __fish_seen_subcommand_from help" -s V -l version -d 'Print version'
complete -c uv -n "__fish_seen_subcommand_from tool; and not __fish_seen_subcommand_from run; and not __fish_seen_subcommand_from help" -f -a "run" -d 'Run a tool'
complete -c uv -n "__fish_seen_subcommand_from tool; and not __fish_seen_subcommand_from run; and not __fish_seen_subcommand_from help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c uv -n "__fish_seen_subcommand_from tool; and __fish_seen_subcommand_from run" -l from -d 'Use the given package to provide the command' -r
complete -c uv -n "__fish_seen_subcommand_from tool; and __fish_seen_subcommand_from run" -l with -d 'Include the following extra requirements' -r
complete -c uv -n "__fish_seen_subcommand_from tool; and __fish_seen_subcommand_from run" -s i -l index-url -d 'The URL of the Python package index (by default: <https://pypi.org/simple>)' -r
complete -c uv -n "__fish_seen_subcommand_from tool; and __fish_seen_subcommand_from run" -l extra-index-url -d 'Extra URLs of package indexes to use, in addition to `--index-url`' -r
complete -c uv -n "__fish_seen_subcommand_from tool; and __fish_seen_subcommand_from run" -s f -l find-links -d 'Locations to search for candidate distributions, beyond those found in the indexes' -r
complete -c uv -n "__fish_seen_subcommand_from tool; and __fish_seen_subcommand_from run" -s P -l upgrade-package -d 'Allow upgrades for a specific package, ignoring pinned versions in any existing output file' -r
complete -c uv -n "__fish_seen_subcommand_from tool; and __fish_seen_subcommand_from run" -l reinstall-package -d 'Reinstall a specific package, regardless of whether it\'s already installed' -r
complete -c uv -n "__fish_seen_subcommand_from tool; and __fish_seen_subcommand_from run" -l index-strategy -d 'The strategy to use when resolving against multiple index URLs' -r -f -a "{first-index	'Only use results from the first index that returns a match for a given package name',unsafe-first-match	'Search for every package name across all indexes, exhausting the versions from the first index before moving on to the next',unsafe-best-match	'Search for every package name across all indexes, preferring the "best" version found. If a package version is in multiple indexes, only look at the entry for the first index'}"
complete -c uv -n "__fish_seen_subcommand_from tool; and __fish_seen_subcommand_from run" -l keyring-provider -d 'Attempt to use `keyring` for authentication for index URLs' -r -f -a "{disabled	'Do not use keyring for credential lookup',subprocess	'Use the `keyring` command for credential lookup'}"
complete -c uv -n "__fish_seen_subcommand_from tool; and __fish_seen_subcommand_from run" -l resolution -d 'The strategy to use when selecting between the different compatible versions for a given package requirement' -r -f -a "{highest	'Resolve the highest compatible version of each package',lowest	'Resolve the lowest compatible version of each package',lowest-direct	'Resolve the lowest compatible version of any direct dependencies, and the highest compatible version of any transitive dependencies'}"
complete -c uv -n "__fish_seen_subcommand_from tool; and __fish_seen_subcommand_from run" -l prerelease -d 'The strategy to use when considering pre-release versions' -r -f -a "{disallow	'Disallow all pre-release versions',allow	'Allow all pre-release versions',if-necessary	'Allow pre-release versions if all versions of a package are pre-release',explicit	'Allow pre-release versions for first-party packages with explicit pre-release markers in their version requirements',if-necessary-or-explicit	'Allow pre-release versions if all versions of a package are pre-release, or if the package has an explicit pre-release marker in its version requirements'}"
complete -c uv -n "__fish_seen_subcommand_from tool; and __fish_seen_subcommand_from run" -s C -l config-setting -d 'Settings to pass to the PEP 517 build backend, specified as `KEY=VALUE` pairs' -r
complete -c uv -n "__fish_seen_subcommand_from tool; and __fish_seen_subcommand_from run" -l exclude-newer -d 'Limit candidate packages to those that were uploaded prior to the given date' -r
complete -c uv -n "__fish_seen_subcommand_from tool; and __fish_seen_subcommand_from run" -l link-mode -d 'The method to use when installing packages from the global cache' -r -f -a "{clone	'Clone (i.e., copy-on-write) packages from the wheel into the site packages',copy	'Copy packages from the wheel into the site packages',hardlink	'Hard link packages from the wheel into the site packages'}"
complete -c uv -n "__fish_seen_subcommand_from tool; and __fish_seen_subcommand_from run" -l no-build-package -d 'Don\'t build source distributions for a specific package' -r
complete -c uv -n "__fish_seen_subcommand_from tool; and __fish_seen_subcommand_from run" -l no-binary-package -d 'Don\'t install pre-built wheels for a specific package' -r
complete -c uv -n "__fish_seen_subcommand_from tool; and __fish_seen_subcommand_from run" -l refresh-package -d 'Refresh cached data for a specific package' -r
complete -c uv -n "__fish_seen_subcommand_from tool; and __fish_seen_subcommand_from run" -s p -l python -d 'The Python interpreter to use to build the run environment.' -r
complete -c uv -n "__fish_seen_subcommand_from tool; and __fish_seen_subcommand_from run" -l color -d 'Control colors in output' -r -f -a "{auto	'Enables colored output only when the output is going to a terminal or TTY with support',always	'Enables colored output regardless of the detected environment',never	'Disables colored output'}"
complete -c uv -n "__fish_seen_subcommand_from tool; and __fish_seen_subcommand_from run" -l cache-dir -d 'Path to the cache directory' -r -F
complete -c uv -n "__fish_seen_subcommand_from tool; and __fish_seen_subcommand_from run" -l config-file -d 'The path to a `uv.toml` file to use for configuration' -r -F
complete -c uv -n "__fish_seen_subcommand_from tool; and __fish_seen_subcommand_from run" -l no-index -d 'Ignore the registry index (e.g., PyPI), instead relying on direct URL dependencies and those discovered via `--find-links`'
complete -c uv -n "__fish_seen_subcommand_from tool; and __fish_seen_subcommand_from run" -s U -l upgrade -d 'Allow package upgrades, ignoring pinned versions in any existing output file'
complete -c uv -n "__fish_seen_subcommand_from tool; and __fish_seen_subcommand_from run" -l no-upgrade
complete -c uv -n "__fish_seen_subcommand_from tool; and __fish_seen_subcommand_from run" -l reinstall -d 'Reinstall all packages, regardless of whether they\'re already installed'
complete -c uv -n "__fish_seen_subcommand_from tool; and __fish_seen_subcommand_from run" -l no-reinstall
complete -c uv -n "__fish_seen_subcommand_from tool; and __fish_seen_subcommand_from run" -l pre
complete -c uv -n "__fish_seen_subcommand_from tool; and __fish_seen_subcommand_from run" -l compile-bytecode -d 'Compile Python files to bytecode'
complete -c uv -n "__fish_seen_subcommand_from tool; and __fish_seen_subcommand_from run" -l no-compile-bytecode
complete -c uv -n "__fish_seen_subcommand_from tool; and __fish_seen_subcommand_from run" -l no-build -d 'Don\'t build source distributions'
complete -c uv -n "__fish_seen_subcommand_from tool; and __fish_seen_subcommand_from run" -l build
complete -c uv -n "__fish_seen_subcommand_from tool; and __fish_seen_subcommand_from run" -l no-binary -d 'Don\'t install pre-built wheels'
complete -c uv -n "__fish_seen_subcommand_from tool; and __fish_seen_subcommand_from run" -l binary
complete -c uv -n "__fish_seen_subcommand_from tool; and __fish_seen_subcommand_from run" -l refresh -d 'Refresh all cached data'
complete -c uv -n "__fish_seen_subcommand_from tool; and __fish_seen_subcommand_from run" -l no-refresh
complete -c uv -n "__fish_seen_subcommand_from tool; and __fish_seen_subcommand_from run" -s q -l quiet -d 'Do not print any output'
complete -c uv -n "__fish_seen_subcommand_from tool; and __fish_seen_subcommand_from run" -s v -l verbose -d 'Use verbose output'
complete -c uv -n "__fish_seen_subcommand_from tool; and __fish_seen_subcommand_from run" -l no-color -d 'Disable colors; provided for compatibility with `pip`'
complete -c uv -n "__fish_seen_subcommand_from tool; and __fish_seen_subcommand_from run" -l native-tls -d 'Whether to load TLS certificates from the platform\'s native certificate store'
complete -c uv -n "__fish_seen_subcommand_from tool; and __fish_seen_subcommand_from run" -l no-native-tls
complete -c uv -n "__fish_seen_subcommand_from tool; and __fish_seen_subcommand_from run" -l offline -d 'Disable network access, relying only on locally cached data and locally available files'
complete -c uv -n "__fish_seen_subcommand_from tool; and __fish_seen_subcommand_from run" -l no-offline
complete -c uv -n "__fish_seen_subcommand_from tool; and __fish_seen_subcommand_from run" -l preview -d 'Whether to enable experimental, preview features'
complete -c uv -n "__fish_seen_subcommand_from tool; and __fish_seen_subcommand_from run" -l no-preview
complete -c uv -n "__fish_seen_subcommand_from tool; and __fish_seen_subcommand_from run" -l isolated -d 'Avoid discovering a `pyproject.toml` or `uv.toml` file in the current directory or any parent directories'
complete -c uv -n "__fish_seen_subcommand_from tool; and __fish_seen_subcommand_from run" -l show-settings -d 'Show the resolved settings for the current command'
complete -c uv -n "__fish_seen_subcommand_from tool; and __fish_seen_subcommand_from run" -s n -l no-cache -d 'Avoid reading from or writing to the cache'
complete -c uv -n "__fish_seen_subcommand_from tool; and __fish_seen_subcommand_from run" -s h -l help -d 'Print help (see more with \'--help\')'
complete -c uv -n "__fish_seen_subcommand_from tool; and __fish_seen_subcommand_from run" -s V -l version -d 'Print version'
complete -c uv -n "__fish_seen_subcommand_from tool; and __fish_seen_subcommand_from help; and not __fish_seen_subcommand_from run; and not __fish_seen_subcommand_from help" -f -a "run" -d 'Run a tool'
complete -c uv -n "__fish_seen_subcommand_from tool; and __fish_seen_subcommand_from help; and not __fish_seen_subcommand_from run; and not __fish_seen_subcommand_from help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c uv -n "__fish_seen_subcommand_from toolchain; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from find; and not __fish_seen_subcommand_from help" -l color -d 'Control colors in output' -r -f -a "{auto	'Enables colored output only when the output is going to a terminal or TTY with support',always	'Enables colored output regardless of the detected environment',never	'Disables colored output'}"
complete -c uv -n "__fish_seen_subcommand_from toolchain; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from find; and not __fish_seen_subcommand_from help" -l cache-dir -d 'Path to the cache directory' -r -F
complete -c uv -n "__fish_seen_subcommand_from toolchain; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from find; and not __fish_seen_subcommand_from help" -l config-file -d 'The path to a `uv.toml` file to use for configuration' -r -F
complete -c uv -n "__fish_seen_subcommand_from toolchain; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from find; and not __fish_seen_subcommand_from help" -s q -l quiet -d 'Do not print any output'
complete -c uv -n "__fish_seen_subcommand_from toolchain; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from find; and not __fish_seen_subcommand_from help" -s v -l verbose -d 'Use verbose output'
complete -c uv -n "__fish_seen_subcommand_from toolchain; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from find; and not __fish_seen_subcommand_from help" -l no-color -d 'Disable colors; provided for compatibility with `pip`'
complete -c uv -n "__fish_seen_subcommand_from toolchain; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from find; and not __fish_seen_subcommand_from help" -l native-tls -d 'Whether to load TLS certificates from the platform\'s native certificate store'
complete -c uv -n "__fish_seen_subcommand_from toolchain; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from find; and not __fish_seen_subcommand_from help" -l no-native-tls
complete -c uv -n "__fish_seen_subcommand_from toolchain; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from find; and not __fish_seen_subcommand_from help" -l offline -d 'Disable network access, relying only on locally cached data and locally available files'
complete -c uv -n "__fish_seen_subcommand_from toolchain; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from find; and not __fish_seen_subcommand_from help" -l no-offline
complete -c uv -n "__fish_seen_subcommand_from toolchain; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from find; and not __fish_seen_subcommand_from help" -l preview -d 'Whether to enable experimental, preview features'
complete -c uv -n "__fish_seen_subcommand_from toolchain; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from find; and not __fish_seen_subcommand_from help" -l no-preview
complete -c uv -n "__fish_seen_subcommand_from toolchain; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from find; and not __fish_seen_subcommand_from help" -l isolated -d 'Avoid discovering a `pyproject.toml` or `uv.toml` file in the current directory or any parent directories'
complete -c uv -n "__fish_seen_subcommand_from toolchain; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from find; and not __fish_seen_subcommand_from help" -l show-settings -d 'Show the resolved settings for the current command'
complete -c uv -n "__fish_seen_subcommand_from toolchain; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from find; and not __fish_seen_subcommand_from help" -s n -l no-cache -d 'Avoid reading from or writing to the cache'
complete -c uv -n "__fish_seen_subcommand_from toolchain; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from find; and not __fish_seen_subcommand_from help" -s h -l help -d 'Print help (see more with \'--help\')'
complete -c uv -n "__fish_seen_subcommand_from toolchain; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from find; and not __fish_seen_subcommand_from help" -s V -l version -d 'Print version'
complete -c uv -n "__fish_seen_subcommand_from toolchain; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from find; and not __fish_seen_subcommand_from help" -f -a "list" -d 'List the available toolchains'
complete -c uv -n "__fish_seen_subcommand_from toolchain; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from find; and not __fish_seen_subcommand_from help" -f -a "install" -d 'Download and install a specific toolchain'
complete -c uv -n "__fish_seen_subcommand_from toolchain; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from find; and not __fish_seen_subcommand_from help" -f -a "find" -d 'Search for a toolchain'
complete -c uv -n "__fish_seen_subcommand_from toolchain; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from find; and not __fish_seen_subcommand_from help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c uv -n "__fish_seen_subcommand_from toolchain; and __fish_seen_subcommand_from list" -l color -d 'Control colors in output' -r -f -a "{auto	'Enables colored output only when the output is going to a terminal or TTY with support',always	'Enables colored output regardless of the detected environment',never	'Disables colored output'}"
complete -c uv -n "__fish_seen_subcommand_from toolchain; and __fish_seen_subcommand_from list" -l cache-dir -d 'Path to the cache directory' -r -F
complete -c uv -n "__fish_seen_subcommand_from toolchain; and __fish_seen_subcommand_from list" -l config-file -d 'The path to a `uv.toml` file to use for configuration' -r -F
complete -c uv -n "__fish_seen_subcommand_from toolchain; and __fish_seen_subcommand_from list" -l all-versions -d 'List all toolchain versions, including outdated patch versions'
complete -c uv -n "__fish_seen_subcommand_from toolchain; and __fish_seen_subcommand_from list" -l all-platforms -d 'List toolchains for all platforms'
complete -c uv -n "__fish_seen_subcommand_from toolchain; and __fish_seen_subcommand_from list" -l only-installed -d 'Only show installed toolchains, exclude available downloads'
complete -c uv -n "__fish_seen_subcommand_from toolchain; and __fish_seen_subcommand_from list" -s q -l quiet -d 'Do not print any output'
complete -c uv -n "__fish_seen_subcommand_from toolchain; and __fish_seen_subcommand_from list" -s v -l verbose -d 'Use verbose output'
complete -c uv -n "__fish_seen_subcommand_from toolchain; and __fish_seen_subcommand_from list" -l no-color -d 'Disable colors; provided for compatibility with `pip`'
complete -c uv -n "__fish_seen_subcommand_from toolchain; and __fish_seen_subcommand_from list" -l native-tls -d 'Whether to load TLS certificates from the platform\'s native certificate store'
complete -c uv -n "__fish_seen_subcommand_from toolchain; and __fish_seen_subcommand_from list" -l no-native-tls
complete -c uv -n "__fish_seen_subcommand_from toolchain; and __fish_seen_subcommand_from list" -l offline -d 'Disable network access, relying only on locally cached data and locally available files'
complete -c uv -n "__fish_seen_subcommand_from toolchain; and __fish_seen_subcommand_from list" -l no-offline
complete -c uv -n "__fish_seen_subcommand_from toolchain; and __fish_seen_subcommand_from list" -l preview -d 'Whether to enable experimental, preview features'
complete -c uv -n "__fish_seen_subcommand_from toolchain; and __fish_seen_subcommand_from list" -l no-preview
complete -c uv -n "__fish_seen_subcommand_from toolchain; and __fish_seen_subcommand_from list" -l isolated -d 'Avoid discovering a `pyproject.toml` or `uv.toml` file in the current directory or any parent directories'
complete -c uv -n "__fish_seen_subcommand_from toolchain; and __fish_seen_subcommand_from list" -l show-settings -d 'Show the resolved settings for the current command'
complete -c uv -n "__fish_seen_subcommand_from toolchain; and __fish_seen_subcommand_from list" -s n -l no-cache -d 'Avoid reading from or writing to the cache'
complete -c uv -n "__fish_seen_subcommand_from toolchain; and __fish_seen_subcommand_from list" -s h -l help -d 'Print help (see more with \'--help\')'
complete -c uv -n "__fish_seen_subcommand_from toolchain; and __fish_seen_subcommand_from list" -s V -l version -d 'Print version'
complete -c uv -n "__fish_seen_subcommand_from toolchain; and __fish_seen_subcommand_from install" -l color -d 'Control colors in output' -r -f -a "{auto	'Enables colored output only when the output is going to a terminal or TTY with support',always	'Enables colored output regardless of the detected environment',never	'Disables colored output'}"
complete -c uv -n "__fish_seen_subcommand_from toolchain; and __fish_seen_subcommand_from install" -l cache-dir -d 'Path to the cache directory' -r -F
complete -c uv -n "__fish_seen_subcommand_from toolchain; and __fish_seen_subcommand_from install" -l config-file -d 'The path to a `uv.toml` file to use for configuration' -r -F
complete -c uv -n "__fish_seen_subcommand_from toolchain; and __fish_seen_subcommand_from install" -s f -l force -d 'Force the installation of the toolchain, even if it is already installed'
complete -c uv -n "__fish_seen_subcommand_from toolchain; and __fish_seen_subcommand_from install" -s q -l quiet -d 'Do not print any output'
complete -c uv -n "__fish_seen_subcommand_from toolchain; and __fish_seen_subcommand_from install" -s v -l verbose -d 'Use verbose output'
complete -c uv -n "__fish_seen_subcommand_from toolchain; and __fish_seen_subcommand_from install" -l no-color -d 'Disable colors; provided for compatibility with `pip`'
complete -c uv -n "__fish_seen_subcommand_from toolchain; and __fish_seen_subcommand_from install" -l native-tls -d 'Whether to load TLS certificates from the platform\'s native certificate store'
complete -c uv -n "__fish_seen_subcommand_from toolchain; and __fish_seen_subcommand_from install" -l no-native-tls
complete -c uv -n "__fish_seen_subcommand_from toolchain; and __fish_seen_subcommand_from install" -l offline -d 'Disable network access, relying only on locally cached data and locally available files'
complete -c uv -n "__fish_seen_subcommand_from toolchain; and __fish_seen_subcommand_from install" -l no-offline
complete -c uv -n "__fish_seen_subcommand_from toolchain; and __fish_seen_subcommand_from install" -l preview -d 'Whether to enable experimental, preview features'
complete -c uv -n "__fish_seen_subcommand_from toolchain; and __fish_seen_subcommand_from install" -l no-preview
complete -c uv -n "__fish_seen_subcommand_from toolchain; and __fish_seen_subcommand_from install" -l isolated -d 'Avoid discovering a `pyproject.toml` or `uv.toml` file in the current directory or any parent directories'
complete -c uv -n "__fish_seen_subcommand_from toolchain; and __fish_seen_subcommand_from install" -l show-settings -d 'Show the resolved settings for the current command'
complete -c uv -n "__fish_seen_subcommand_from toolchain; and __fish_seen_subcommand_from install" -s n -l no-cache -d 'Avoid reading from or writing to the cache'
complete -c uv -n "__fish_seen_subcommand_from toolchain; and __fish_seen_subcommand_from install" -s h -l help -d 'Print help (see more with \'--help\')'
complete -c uv -n "__fish_seen_subcommand_from toolchain; and __fish_seen_subcommand_from install" -s V -l version -d 'Print version'
complete -c uv -n "__fish_seen_subcommand_from toolchain; and __fish_seen_subcommand_from find" -l color -d 'Control colors in output' -r -f -a "{auto	'Enables colored output only when the output is going to a terminal or TTY with support',always	'Enables colored output regardless of the detected environment',never	'Disables colored output'}"
complete -c uv -n "__fish_seen_subcommand_from toolchain; and __fish_seen_subcommand_from find" -l cache-dir -d 'Path to the cache directory' -r -F
complete -c uv -n "__fish_seen_subcommand_from toolchain; and __fish_seen_subcommand_from find" -l config-file -d 'The path to a `uv.toml` file to use for configuration' -r -F
complete -c uv -n "__fish_seen_subcommand_from toolchain; and __fish_seen_subcommand_from find" -s q -l quiet -d 'Do not print any output'
complete -c uv -n "__fish_seen_subcommand_from toolchain; and __fish_seen_subcommand_from find" -s v -l verbose -d 'Use verbose output'
complete -c uv -n "__fish_seen_subcommand_from toolchain; and __fish_seen_subcommand_from find" -l no-color -d 'Disable colors; provided for compatibility with `pip`'
complete -c uv -n "__fish_seen_subcommand_from toolchain; and __fish_seen_subcommand_from find" -l native-tls -d 'Whether to load TLS certificates from the platform\'s native certificate store'
complete -c uv -n "__fish_seen_subcommand_from toolchain; and __fish_seen_subcommand_from find" -l no-native-tls
complete -c uv -n "__fish_seen_subcommand_from toolchain; and __fish_seen_subcommand_from find" -l offline -d 'Disable network access, relying only on locally cached data and locally available files'
complete -c uv -n "__fish_seen_subcommand_from toolchain; and __fish_seen_subcommand_from find" -l no-offline
complete -c uv -n "__fish_seen_subcommand_from toolchain; and __fish_seen_subcommand_from find" -l preview -d 'Whether to enable experimental, preview features'
complete -c uv -n "__fish_seen_subcommand_from toolchain; and __fish_seen_subcommand_from find" -l no-preview
complete -c uv -n "__fish_seen_subcommand_from toolchain; and __fish_seen_subcommand_from find" -l isolated -d 'Avoid discovering a `pyproject.toml` or `uv.toml` file in the current directory or any parent directories'
complete -c uv -n "__fish_seen_subcommand_from toolchain; and __fish_seen_subcommand_from find" -l show-settings -d 'Show the resolved settings for the current command'
complete -c uv -n "__fish_seen_subcommand_from toolchain; and __fish_seen_subcommand_from find" -s n -l no-cache -d 'Avoid reading from or writing to the cache'
complete -c uv -n "__fish_seen_subcommand_from toolchain; and __fish_seen_subcommand_from find" -s h -l help -d 'Print help (see more with \'--help\')'
complete -c uv -n "__fish_seen_subcommand_from toolchain; and __fish_seen_subcommand_from help; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from find; and not __fish_seen_subcommand_from help" -f -a "list" -d 'List the available toolchains'
complete -c uv -n "__fish_seen_subcommand_from toolchain; and __fish_seen_subcommand_from help; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from find; and not __fish_seen_subcommand_from help" -f -a "install" -d 'Download and install a specific toolchain'
complete -c uv -n "__fish_seen_subcommand_from toolchain; and __fish_seen_subcommand_from help; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from find; and not __fish_seen_subcommand_from help" -f -a "find" -d 'Search for a toolchain'
complete -c uv -n "__fish_seen_subcommand_from toolchain; and __fish_seen_subcommand_from help; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from find; and not __fish_seen_subcommand_from help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c uv -n "__fish_seen_subcommand_from run" -l extra -d 'Include optional dependencies from the extra group name; may be provided more than once. Only applies to `pyproject.toml`, `setup.py`, and `setup.cfg` sources' -r
complete -c uv -n "__fish_seen_subcommand_from run" -l with -d 'Run with the given packages installed' -r
complete -c uv -n "__fish_seen_subcommand_from run" -s i -l index-url -d 'The URL of the Python package index (by default: <https://pypi.org/simple>)' -r
complete -c uv -n "__fish_seen_subcommand_from run" -l extra-index-url -d 'Extra URLs of package indexes to use, in addition to `--index-url`' -r
complete -c uv -n "__fish_seen_subcommand_from run" -s f -l find-links -d 'Locations to search for candidate distributions, beyond those found in the indexes' -r
complete -c uv -n "__fish_seen_subcommand_from run" -s P -l upgrade-package -d 'Allow upgrades for a specific package, ignoring pinned versions in any existing output file' -r
complete -c uv -n "__fish_seen_subcommand_from run" -l reinstall-package -d 'Reinstall a specific package, regardless of whether it\'s already installed' -r
complete -c uv -n "__fish_seen_subcommand_from run" -l index-strategy -d 'The strategy to use when resolving against multiple index URLs' -r -f -a "{first-index	'Only use results from the first index that returns a match for a given package name',unsafe-first-match	'Search for every package name across all indexes, exhausting the versions from the first index before moving on to the next',unsafe-best-match	'Search for every package name across all indexes, preferring the "best" version found. If a package version is in multiple indexes, only look at the entry for the first index'}"
complete -c uv -n "__fish_seen_subcommand_from run" -l keyring-provider -d 'Attempt to use `keyring` for authentication for index URLs' -r -f -a "{disabled	'Do not use keyring for credential lookup',subprocess	'Use the `keyring` command for credential lookup'}"
complete -c uv -n "__fish_seen_subcommand_from run" -l resolution -d 'The strategy to use when selecting between the different compatible versions for a given package requirement' -r -f -a "{highest	'Resolve the highest compatible version of each package',lowest	'Resolve the lowest compatible version of each package',lowest-direct	'Resolve the lowest compatible version of any direct dependencies, and the highest compatible version of any transitive dependencies'}"
complete -c uv -n "__fish_seen_subcommand_from run" -l prerelease -d 'The strategy to use when considering pre-release versions' -r -f -a "{disallow	'Disallow all pre-release versions',allow	'Allow all pre-release versions',if-necessary	'Allow pre-release versions if all versions of a package are pre-release',explicit	'Allow pre-release versions for first-party packages with explicit pre-release markers in their version requirements',if-necessary-or-explicit	'Allow pre-release versions if all versions of a package are pre-release, or if the package has an explicit pre-release marker in its version requirements'}"
complete -c uv -n "__fish_seen_subcommand_from run" -s C -l config-setting -d 'Settings to pass to the PEP 517 build backend, specified as `KEY=VALUE` pairs' -r
complete -c uv -n "__fish_seen_subcommand_from run" -l exclude-newer -d 'Limit candidate packages to those that were uploaded prior to the given date' -r
complete -c uv -n "__fish_seen_subcommand_from run" -l link-mode -d 'The method to use when installing packages from the global cache' -r -f -a "{clone	'Clone (i.e., copy-on-write) packages from the wheel into the site packages',copy	'Copy packages from the wheel into the site packages',hardlink	'Hard link packages from the wheel into the site packages'}"
complete -c uv -n "__fish_seen_subcommand_from run" -l no-build-package -d 'Don\'t build source distributions for a specific package' -r
complete -c uv -n "__fish_seen_subcommand_from run" -l no-binary-package -d 'Don\'t install pre-built wheels for a specific package' -r
complete -c uv -n "__fish_seen_subcommand_from run" -l refresh-package -d 'Refresh cached data for a specific package' -r
complete -c uv -n "__fish_seen_subcommand_from run" -s p -l python -d 'The Python interpreter to use to build the run environment.' -r
complete -c uv -n "__fish_seen_subcommand_from run" -l package -d 'Run the command in a different package in the workspace' -r
complete -c uv -n "__fish_seen_subcommand_from run" -l color -d 'Control colors in output' -r -f -a "{auto	'Enables colored output only when the output is going to a terminal or TTY with support',always	'Enables colored output regardless of the detected environment',never	'Disables colored output'}"
complete -c uv -n "__fish_seen_subcommand_from run" -l cache-dir -d 'Path to the cache directory' -r -F
complete -c uv -n "__fish_seen_subcommand_from run" -l config-file -d 'The path to a `uv.toml` file to use for configuration' -r -F
complete -c uv -n "__fish_seen_subcommand_from run" -l all-extras -d 'Include all optional dependencies. Only applies to `pyproject.toml`, `setup.py`, and `setup.cfg` sources'
complete -c uv -n "__fish_seen_subcommand_from run" -l no-all-extras
complete -c uv -n "__fish_seen_subcommand_from run" -l dev -d 'Include development dependencies'
complete -c uv -n "__fish_seen_subcommand_from run" -l no-dev -d 'Omit development dependencies'
complete -c uv -n "__fish_seen_subcommand_from run" -l no-index -d 'Ignore the registry index (e.g., PyPI), instead relying on direct URL dependencies and those discovered via `--find-links`'
complete -c uv -n "__fish_seen_subcommand_from run" -s U -l upgrade -d 'Allow package upgrades, ignoring pinned versions in any existing output file'
complete -c uv -n "__fish_seen_subcommand_from run" -l no-upgrade
complete -c uv -n "__fish_seen_subcommand_from run" -l reinstall -d 'Reinstall all packages, regardless of whether they\'re already installed'
complete -c uv -n "__fish_seen_subcommand_from run" -l no-reinstall
complete -c uv -n "__fish_seen_subcommand_from run" -l pre
complete -c uv -n "__fish_seen_subcommand_from run" -l compile-bytecode -d 'Compile Python files to bytecode'
complete -c uv -n "__fish_seen_subcommand_from run" -l no-compile-bytecode
complete -c uv -n "__fish_seen_subcommand_from run" -l no-build -d 'Don\'t build source distributions'
complete -c uv -n "__fish_seen_subcommand_from run" -l build
complete -c uv -n "__fish_seen_subcommand_from run" -l no-binary -d 'Don\'t install pre-built wheels'
complete -c uv -n "__fish_seen_subcommand_from run" -l binary
complete -c uv -n "__fish_seen_subcommand_from run" -l refresh -d 'Refresh all cached data'
complete -c uv -n "__fish_seen_subcommand_from run" -l no-refresh
complete -c uv -n "__fish_seen_subcommand_from run" -s q -l quiet -d 'Do not print any output'
complete -c uv -n "__fish_seen_subcommand_from run" -s v -l verbose -d 'Use verbose output'
complete -c uv -n "__fish_seen_subcommand_from run" -l no-color -d 'Disable colors; provided for compatibility with `pip`'
complete -c uv -n "__fish_seen_subcommand_from run" -l native-tls -d 'Whether to load TLS certificates from the platform\'s native certificate store'
complete -c uv -n "__fish_seen_subcommand_from run" -l no-native-tls
complete -c uv -n "__fish_seen_subcommand_from run" -l offline -d 'Disable network access, relying only on locally cached data and locally available files'
complete -c uv -n "__fish_seen_subcommand_from run" -l no-offline
complete -c uv -n "__fish_seen_subcommand_from run" -l preview -d 'Whether to enable experimental, preview features'
complete -c uv -n "__fish_seen_subcommand_from run" -l no-preview
complete -c uv -n "__fish_seen_subcommand_from run" -l isolated -d 'Avoid discovering a `pyproject.toml` or `uv.toml` file in the current directory or any parent directories'
complete -c uv -n "__fish_seen_subcommand_from run" -l show-settings -d 'Show the resolved settings for the current command'
complete -c uv -n "__fish_seen_subcommand_from run" -s n -l no-cache -d 'Avoid reading from or writing to the cache'
complete -c uv -n "__fish_seen_subcommand_from run" -s h -l help -d 'Print help (see more with \'--help\')'
complete -c uv -n "__fish_seen_subcommand_from run" -s V -l version -d 'Print version'
complete -c uv -n "__fish_seen_subcommand_from sync" -l extra -d 'Include optional dependencies from the extra group name; may be provided more than once. Only applies to `pyproject.toml`, `setup.py`, and `setup.cfg` sources' -r
complete -c uv -n "__fish_seen_subcommand_from sync" -s i -l index-url -d 'The URL of the Python package index (by default: <https://pypi.org/simple>)' -r
complete -c uv -n "__fish_seen_subcommand_from sync" -l extra-index-url -d 'Extra URLs of package indexes to use, in addition to `--index-url`' -r
complete -c uv -n "__fish_seen_subcommand_from sync" -s f -l find-links -d 'Locations to search for candidate distributions, beyond those found in the indexes' -r
complete -c uv -n "__fish_seen_subcommand_from sync" -l reinstall-package -d 'Reinstall a specific package, regardless of whether it\'s already installed' -r
complete -c uv -n "__fish_seen_subcommand_from sync" -l index-strategy -d 'The strategy to use when resolving against multiple index URLs' -r -f -a "{first-index	'Only use results from the first index that returns a match for a given package name',unsafe-first-match	'Search for every package name across all indexes, exhausting the versions from the first index before moving on to the next',unsafe-best-match	'Search for every package name across all indexes, preferring the "best" version found. If a package version is in multiple indexes, only look at the entry for the first index'}"
complete -c uv -n "__fish_seen_subcommand_from sync" -l keyring-provider -d 'Attempt to use `keyring` for authentication for index URLs' -r -f -a "{disabled	'Do not use keyring for credential lookup',subprocess	'Use the `keyring` command for credential lookup'}"
complete -c uv -n "__fish_seen_subcommand_from sync" -s C -l config-setting -d 'Settings to pass to the PEP 517 build backend, specified as `KEY=VALUE` pairs' -r
complete -c uv -n "__fish_seen_subcommand_from sync" -l link-mode -d 'The method to use when installing packages from the global cache' -r -f -a "{clone	'Clone (i.e., copy-on-write) packages from the wheel into the site packages',copy	'Copy packages from the wheel into the site packages',hardlink	'Hard link packages from the wheel into the site packages'}"
complete -c uv -n "__fish_seen_subcommand_from sync" -l no-build-package -d 'Don\'t build source distributions for a specific package' -r
complete -c uv -n "__fish_seen_subcommand_from sync" -l no-binary-package -d 'Don\'t install pre-built wheels for a specific package' -r
complete -c uv -n "__fish_seen_subcommand_from sync" -l refresh-package -d 'Refresh cached data for a specific package' -r
complete -c uv -n "__fish_seen_subcommand_from sync" -s p -l python -d 'The Python interpreter to use to build the run environment.' -r
complete -c uv -n "__fish_seen_subcommand_from sync" -l color -d 'Control colors in output' -r -f -a "{auto	'Enables colored output only when the output is going to a terminal or TTY with support',always	'Enables colored output regardless of the detected environment',never	'Disables colored output'}"
complete -c uv -n "__fish_seen_subcommand_from sync" -l cache-dir -d 'Path to the cache directory' -r -F
complete -c uv -n "__fish_seen_subcommand_from sync" -l config-file -d 'The path to a `uv.toml` file to use for configuration' -r -F
complete -c uv -n "__fish_seen_subcommand_from sync" -l all-extras -d 'Include all optional dependencies. Only applies to `pyproject.toml`, `setup.py`, and `setup.cfg` sources'
complete -c uv -n "__fish_seen_subcommand_from sync" -l no-all-extras
complete -c uv -n "__fish_seen_subcommand_from sync" -l dev -d 'Include development dependencies'
complete -c uv -n "__fish_seen_subcommand_from sync" -l no-dev -d 'Omit development dependencies'
complete -c uv -n "__fish_seen_subcommand_from sync" -l no-clean -d 'Does not clean the environment. Without this flag any extraneous installations will be removed'
complete -c uv -n "__fish_seen_subcommand_from sync" -l no-index -d 'Ignore the registry index (e.g., PyPI), instead relying on direct URL dependencies and those discovered via `--find-links`'
complete -c uv -n "__fish_seen_subcommand_from sync" -l reinstall -d 'Reinstall all packages, regardless of whether they\'re already installed'
complete -c uv -n "__fish_seen_subcommand_from sync" -l no-reinstall
complete -c uv -n "__fish_seen_subcommand_from sync" -l compile-bytecode -d 'Compile Python files to bytecode'
complete -c uv -n "__fish_seen_subcommand_from sync" -l no-compile-bytecode
complete -c uv -n "__fish_seen_subcommand_from sync" -l no-build -d 'Don\'t build source distributions'
complete -c uv -n "__fish_seen_subcommand_from sync" -l build
complete -c uv -n "__fish_seen_subcommand_from sync" -l no-binary -d 'Don\'t install pre-built wheels'
complete -c uv -n "__fish_seen_subcommand_from sync" -l binary
complete -c uv -n "__fish_seen_subcommand_from sync" -l refresh -d 'Refresh all cached data'
complete -c uv -n "__fish_seen_subcommand_from sync" -l no-refresh
complete -c uv -n "__fish_seen_subcommand_from sync" -s q -l quiet -d 'Do not print any output'
complete -c uv -n "__fish_seen_subcommand_from sync" -s v -l verbose -d 'Use verbose output'
complete -c uv -n "__fish_seen_subcommand_from sync" -l no-color -d 'Disable colors; provided for compatibility with `pip`'
complete -c uv -n "__fish_seen_subcommand_from sync" -l native-tls -d 'Whether to load TLS certificates from the platform\'s native certificate store'
complete -c uv -n "__fish_seen_subcommand_from sync" -l no-native-tls
complete -c uv -n "__fish_seen_subcommand_from sync" -l offline -d 'Disable network access, relying only on locally cached data and locally available files'
complete -c uv -n "__fish_seen_subcommand_from sync" -l no-offline
complete -c uv -n "__fish_seen_subcommand_from sync" -l preview -d 'Whether to enable experimental, preview features'
complete -c uv -n "__fish_seen_subcommand_from sync" -l no-preview
complete -c uv -n "__fish_seen_subcommand_from sync" -l isolated -d 'Avoid discovering a `pyproject.toml` or `uv.toml` file in the current directory or any parent directories'
complete -c uv -n "__fish_seen_subcommand_from sync" -l show-settings -d 'Show the resolved settings for the current command'
complete -c uv -n "__fish_seen_subcommand_from sync" -s n -l no-cache -d 'Avoid reading from or writing to the cache'
complete -c uv -n "__fish_seen_subcommand_from sync" -s h -l help -d 'Print help (see more with \'--help\')'
complete -c uv -n "__fish_seen_subcommand_from sync" -s V -l version -d 'Print version'
complete -c uv -n "__fish_seen_subcommand_from lock" -s i -l index-url -d 'The URL of the Python package index (by default: <https://pypi.org/simple>)' -r
complete -c uv -n "__fish_seen_subcommand_from lock" -l extra-index-url -d 'Extra URLs of package indexes to use, in addition to `--index-url`' -r
complete -c uv -n "__fish_seen_subcommand_from lock" -s f -l find-links -d 'Locations to search for candidate distributions, beyond those found in the indexes' -r
complete -c uv -n "__fish_seen_subcommand_from lock" -s P -l upgrade-package -d 'Allow upgrades for a specific package, ignoring pinned versions in any existing output file' -r
complete -c uv -n "__fish_seen_subcommand_from lock" -l index-strategy -d 'The strategy to use when resolving against multiple index URLs' -r -f -a "{first-index	'Only use results from the first index that returns a match for a given package name',unsafe-first-match	'Search for every package name across all indexes, exhausting the versions from the first index before moving on to the next',unsafe-best-match	'Search for every package name across all indexes, preferring the "best" version found. If a package version is in multiple indexes, only look at the entry for the first index'}"
complete -c uv -n "__fish_seen_subcommand_from lock" -l keyring-provider -d 'Attempt to use `keyring` for authentication for index URLs' -r -f -a "{disabled	'Do not use keyring for credential lookup',subprocess	'Use the `keyring` command for credential lookup'}"
complete -c uv -n "__fish_seen_subcommand_from lock" -l resolution -d 'The strategy to use when selecting between the different compatible versions for a given package requirement' -r -f -a "{highest	'Resolve the highest compatible version of each package',lowest	'Resolve the lowest compatible version of each package',lowest-direct	'Resolve the lowest compatible version of any direct dependencies, and the highest compatible version of any transitive dependencies'}"
complete -c uv -n "__fish_seen_subcommand_from lock" -l prerelease -d 'The strategy to use when considering pre-release versions' -r -f -a "{disallow	'Disallow all pre-release versions',allow	'Allow all pre-release versions',if-necessary	'Allow pre-release versions if all versions of a package are pre-release',explicit	'Allow pre-release versions for first-party packages with explicit pre-release markers in their version requirements',if-necessary-or-explicit	'Allow pre-release versions if all versions of a package are pre-release, or if the package has an explicit pre-release marker in its version requirements'}"
complete -c uv -n "__fish_seen_subcommand_from lock" -s C -l config-setting -d 'Settings to pass to the PEP 517 build backend, specified as `KEY=VALUE` pairs' -r
complete -c uv -n "__fish_seen_subcommand_from lock" -l exclude-newer -d 'Limit candidate packages to those that were uploaded prior to the given date' -r
complete -c uv -n "__fish_seen_subcommand_from lock" -l link-mode -d 'The method to use when installing packages from the global cache' -r -f -a "{clone	'Clone (i.e., copy-on-write) packages from the wheel into the site packages',copy	'Copy packages from the wheel into the site packages',hardlink	'Hard link packages from the wheel into the site packages'}"
complete -c uv -n "__fish_seen_subcommand_from lock" -l no-build-package -d 'Don\'t build source distributions for a specific package' -r
complete -c uv -n "__fish_seen_subcommand_from lock" -l no-binary-package -d 'Don\'t install pre-built wheels for a specific package' -r
complete -c uv -n "__fish_seen_subcommand_from lock" -l refresh-package -d 'Refresh cached data for a specific package' -r
complete -c uv -n "__fish_seen_subcommand_from lock" -s p -l python -d 'The Python interpreter to use to build the run environment.' -r
complete -c uv -n "__fish_seen_subcommand_from lock" -l color -d 'Control colors in output' -r -f -a "{auto	'Enables colored output only when the output is going to a terminal or TTY with support',always	'Enables colored output regardless of the detected environment',never	'Disables colored output'}"
complete -c uv -n "__fish_seen_subcommand_from lock" -l cache-dir -d 'Path to the cache directory' -r -F
complete -c uv -n "__fish_seen_subcommand_from lock" -l config-file -d 'The path to a `uv.toml` file to use for configuration' -r -F
complete -c uv -n "__fish_seen_subcommand_from lock" -l no-index -d 'Ignore the registry index (e.g., PyPI), instead relying on direct URL dependencies and those discovered via `--find-links`'
complete -c uv -n "__fish_seen_subcommand_from lock" -s U -l upgrade -d 'Allow package upgrades, ignoring pinned versions in any existing output file'
complete -c uv -n "__fish_seen_subcommand_from lock" -l no-upgrade
complete -c uv -n "__fish_seen_subcommand_from lock" -l pre
complete -c uv -n "__fish_seen_subcommand_from lock" -l no-build -d 'Don\'t build source distributions'
complete -c uv -n "__fish_seen_subcommand_from lock" -l build
complete -c uv -n "__fish_seen_subcommand_from lock" -l no-binary -d 'Don\'t install pre-built wheels'
complete -c uv -n "__fish_seen_subcommand_from lock" -l binary
complete -c uv -n "__fish_seen_subcommand_from lock" -l refresh -d 'Refresh all cached data'
complete -c uv -n "__fish_seen_subcommand_from lock" -l no-refresh
complete -c uv -n "__fish_seen_subcommand_from lock" -s q -l quiet -d 'Do not print any output'
complete -c uv -n "__fish_seen_subcommand_from lock" -s v -l verbose -d 'Use verbose output'
complete -c uv -n "__fish_seen_subcommand_from lock" -l no-color -d 'Disable colors; provided for compatibility with `pip`'
complete -c uv -n "__fish_seen_subcommand_from lock" -l native-tls -d 'Whether to load TLS certificates from the platform\'s native certificate store'
complete -c uv -n "__fish_seen_subcommand_from lock" -l no-native-tls
complete -c uv -n "__fish_seen_subcommand_from lock" -l offline -d 'Disable network access, relying only on locally cached data and locally available files'
complete -c uv -n "__fish_seen_subcommand_from lock" -l no-offline
complete -c uv -n "__fish_seen_subcommand_from lock" -l preview -d 'Whether to enable experimental, preview features'
complete -c uv -n "__fish_seen_subcommand_from lock" -l no-preview
complete -c uv -n "__fish_seen_subcommand_from lock" -l isolated -d 'Avoid discovering a `pyproject.toml` or `uv.toml` file in the current directory or any parent directories'
complete -c uv -n "__fish_seen_subcommand_from lock" -l show-settings -d 'Show the resolved settings for the current command'
complete -c uv -n "__fish_seen_subcommand_from lock" -s n -l no-cache -d 'Avoid reading from or writing to the cache'
complete -c uv -n "__fish_seen_subcommand_from lock" -s h -l help -d 'Print help (see more with \'--help\')'
complete -c uv -n "__fish_seen_subcommand_from lock" -s V -l version -d 'Print version'
complete -c uv -n "__fish_seen_subcommand_from add" -s i -l index-url -d 'The URL of the Python package index (by default: <https://pypi.org/simple>)' -r
complete -c uv -n "__fish_seen_subcommand_from add" -l extra-index-url -d 'Extra URLs of package indexes to use, in addition to `--index-url`' -r
complete -c uv -n "__fish_seen_subcommand_from add" -s f -l find-links -d 'Locations to search for candidate distributions, beyond those found in the indexes' -r
complete -c uv -n "__fish_seen_subcommand_from add" -s P -l upgrade-package -d 'Allow upgrades for a specific package, ignoring pinned versions in any existing output file' -r
complete -c uv -n "__fish_seen_subcommand_from add" -l reinstall-package -d 'Reinstall a specific package, regardless of whether it\'s already installed' -r
complete -c uv -n "__fish_seen_subcommand_from add" -l index-strategy -d 'The strategy to use when resolving against multiple index URLs' -r -f -a "{first-index	'Only use results from the first index that returns a match for a given package name',unsafe-first-match	'Search for every package name across all indexes, exhausting the versions from the first index before moving on to the next',unsafe-best-match	'Search for every package name across all indexes, preferring the "best" version found. If a package version is in multiple indexes, only look at the entry for the first index'}"
complete -c uv -n "__fish_seen_subcommand_from add" -l keyring-provider -d 'Attempt to use `keyring` for authentication for index URLs' -r -f -a "{disabled	'Do not use keyring for credential lookup',subprocess	'Use the `keyring` command for credential lookup'}"
complete -c uv -n "__fish_seen_subcommand_from add" -l resolution -d 'The strategy to use when selecting between the different compatible versions for a given package requirement' -r -f -a "{highest	'Resolve the highest compatible version of each package',lowest	'Resolve the lowest compatible version of each package',lowest-direct	'Resolve the lowest compatible version of any direct dependencies, and the highest compatible version of any transitive dependencies'}"
complete -c uv -n "__fish_seen_subcommand_from add" -l prerelease -d 'The strategy to use when considering pre-release versions' -r -f -a "{disallow	'Disallow all pre-release versions',allow	'Allow all pre-release versions',if-necessary	'Allow pre-release versions if all versions of a package are pre-release',explicit	'Allow pre-release versions for first-party packages with explicit pre-release markers in their version requirements',if-necessary-or-explicit	'Allow pre-release versions if all versions of a package are pre-release, or if the package has an explicit pre-release marker in its version requirements'}"
complete -c uv -n "__fish_seen_subcommand_from add" -s C -l config-setting -d 'Settings to pass to the PEP 517 build backend, specified as `KEY=VALUE` pairs' -r
complete -c uv -n "__fish_seen_subcommand_from add" -l exclude-newer -d 'Limit candidate packages to those that were uploaded prior to the given date' -r
complete -c uv -n "__fish_seen_subcommand_from add" -l link-mode -d 'The method to use when installing packages from the global cache' -r -f -a "{clone	'Clone (i.e., copy-on-write) packages from the wheel into the site packages',copy	'Copy packages from the wheel into the site packages',hardlink	'Hard link packages from the wheel into the site packages'}"
complete -c uv -n "__fish_seen_subcommand_from add" -l no-build-package -d 'Don\'t build source distributions for a specific package' -r
complete -c uv -n "__fish_seen_subcommand_from add" -l no-binary-package -d 'Don\'t install pre-built wheels for a specific package' -r
complete -c uv -n "__fish_seen_subcommand_from add" -l refresh-package -d 'Refresh cached data for a specific package' -r
complete -c uv -n "__fish_seen_subcommand_from add" -s p -l python -d 'The Python interpreter into which packages should be installed.' -r
complete -c uv -n "__fish_seen_subcommand_from add" -l color -d 'Control colors in output' -r -f -a "{auto	'Enables colored output only when the output is going to a terminal or TTY with support',always	'Enables colored output regardless of the detected environment',never	'Disables colored output'}"
complete -c uv -n "__fish_seen_subcommand_from add" -l cache-dir -d 'Path to the cache directory' -r -F
complete -c uv -n "__fish_seen_subcommand_from add" -l config-file -d 'The path to a `uv.toml` file to use for configuration' -r -F
complete -c uv -n "__fish_seen_subcommand_from add" -l dev -d 'Add the requirements as development dependencies'
complete -c uv -n "__fish_seen_subcommand_from add" -l workspace -d 'Add the requirements as workspace dependencies'
complete -c uv -n "__fish_seen_subcommand_from add" -l no-index -d 'Ignore the registry index (e.g., PyPI), instead relying on direct URL dependencies and those discovered via `--find-links`'
complete -c uv -n "__fish_seen_subcommand_from add" -s U -l upgrade -d 'Allow package upgrades, ignoring pinned versions in any existing output file'
complete -c uv -n "__fish_seen_subcommand_from add" -l no-upgrade
complete -c uv -n "__fish_seen_subcommand_from add" -l reinstall -d 'Reinstall all packages, regardless of whether they\'re already installed'
complete -c uv -n "__fish_seen_subcommand_from add" -l no-reinstall
complete -c uv -n "__fish_seen_subcommand_from add" -l pre
complete -c uv -n "__fish_seen_subcommand_from add" -l compile-bytecode -d 'Compile Python files to bytecode'
complete -c uv -n "__fish_seen_subcommand_from add" -l no-compile-bytecode
complete -c uv -n "__fish_seen_subcommand_from add" -l no-build -d 'Don\'t build source distributions'
complete -c uv -n "__fish_seen_subcommand_from add" -l build
complete -c uv -n "__fish_seen_subcommand_from add" -l no-binary -d 'Don\'t install pre-built wheels'
complete -c uv -n "__fish_seen_subcommand_from add" -l binary
complete -c uv -n "__fish_seen_subcommand_from add" -l refresh -d 'Refresh all cached data'
complete -c uv -n "__fish_seen_subcommand_from add" -l no-refresh
complete -c uv -n "__fish_seen_subcommand_from add" -s q -l quiet -d 'Do not print any output'
complete -c uv -n "__fish_seen_subcommand_from add" -s v -l verbose -d 'Use verbose output'
complete -c uv -n "__fish_seen_subcommand_from add" -l no-color -d 'Disable colors; provided for compatibility with `pip`'
complete -c uv -n "__fish_seen_subcommand_from add" -l native-tls -d 'Whether to load TLS certificates from the platform\'s native certificate store'
complete -c uv -n "__fish_seen_subcommand_from add" -l no-native-tls
complete -c uv -n "__fish_seen_subcommand_from add" -l offline -d 'Disable network access, relying only on locally cached data and locally available files'
complete -c uv -n "__fish_seen_subcommand_from add" -l no-offline
complete -c uv -n "__fish_seen_subcommand_from add" -l preview -d 'Whether to enable experimental, preview features'
complete -c uv -n "__fish_seen_subcommand_from add" -l no-preview
complete -c uv -n "__fish_seen_subcommand_from add" -l isolated -d 'Avoid discovering a `pyproject.toml` or `uv.toml` file in the current directory or any parent directories'
complete -c uv -n "__fish_seen_subcommand_from add" -l show-settings -d 'Show the resolved settings for the current command'
complete -c uv -n "__fish_seen_subcommand_from add" -s n -l no-cache -d 'Avoid reading from or writing to the cache'
complete -c uv -n "__fish_seen_subcommand_from add" -s h -l help -d 'Print help (see more with \'--help\')'
complete -c uv -n "__fish_seen_subcommand_from add" -s V -l version -d 'Print version'
complete -c uv -n "__fish_seen_subcommand_from remove" -s p -l python -d 'The Python interpreter into which packages should be installed.' -r
complete -c uv -n "__fish_seen_subcommand_from remove" -l color -d 'Control colors in output' -r -f -a "{auto	'Enables colored output only when the output is going to a terminal or TTY with support',always	'Enables colored output regardless of the detected environment',never	'Disables colored output'}"
complete -c uv -n "__fish_seen_subcommand_from remove" -l cache-dir -d 'Path to the cache directory' -r -F
complete -c uv -n "__fish_seen_subcommand_from remove" -l config-file -d 'The path to a `uv.toml` file to use for configuration' -r -F
complete -c uv -n "__fish_seen_subcommand_from remove" -l dev -d 'Remove the requirements from development dependencies'
complete -c uv -n "__fish_seen_subcommand_from remove" -s q -l quiet -d 'Do not print any output'
complete -c uv -n "__fish_seen_subcommand_from remove" -s v -l verbose -d 'Use verbose output'
complete -c uv -n "__fish_seen_subcommand_from remove" -l no-color -d 'Disable colors; provided for compatibility with `pip`'
complete -c uv -n "__fish_seen_subcommand_from remove" -l native-tls -d 'Whether to load TLS certificates from the platform\'s native certificate store'
complete -c uv -n "__fish_seen_subcommand_from remove" -l no-native-tls
complete -c uv -n "__fish_seen_subcommand_from remove" -l offline -d 'Disable network access, relying only on locally cached data and locally available files'
complete -c uv -n "__fish_seen_subcommand_from remove" -l no-offline
complete -c uv -n "__fish_seen_subcommand_from remove" -l preview -d 'Whether to enable experimental, preview features'
complete -c uv -n "__fish_seen_subcommand_from remove" -l no-preview
complete -c uv -n "__fish_seen_subcommand_from remove" -l isolated -d 'Avoid discovering a `pyproject.toml` or `uv.toml` file in the current directory or any parent directories'
complete -c uv -n "__fish_seen_subcommand_from remove" -l show-settings -d 'Show the resolved settings for the current command'
complete -c uv -n "__fish_seen_subcommand_from remove" -s n -l no-cache -d 'Avoid reading from or writing to the cache'
complete -c uv -n "__fish_seen_subcommand_from remove" -s h -l help -d 'Print help (see more with \'--help\')'
complete -c uv -n "__fish_seen_subcommand_from remove" -s V -l version -d 'Print version'
complete -c uv -n "__fish_seen_subcommand_from venv" -s p -l python -d 'The Python interpreter to use for the virtual environment.' -r
complete -c uv -n "__fish_seen_subcommand_from venv" -l prompt -d 'Provide an alternative prompt prefix for the virtual environment.' -r
complete -c uv -n "__fish_seen_subcommand_from venv" -s i -l index-url -d 'The URL of the Python package index (by default: <https://pypi.org/simple>)' -r
complete -c uv -n "__fish_seen_subcommand_from venv" -l extra-index-url -d 'Extra URLs of package indexes to use, in addition to `--index-url`' -r
complete -c uv -n "__fish_seen_subcommand_from venv" -s f -l find-links -d 'Locations to search for candidate distributions, beyond those found in the indexes' -r
complete -c uv -n "__fish_seen_subcommand_from venv" -l index-strategy -d 'The strategy to use when resolving against multiple index URLs' -r -f -a "{first-index	'Only use results from the first index that returns a match for a given package name',unsafe-first-match	'Search for every package name across all indexes, exhausting the versions from the first index before moving on to the next',unsafe-best-match	'Search for every package name across all indexes, preferring the "best" version found. If a package version is in multiple indexes, only look at the entry for the first index'}"
complete -c uv -n "__fish_seen_subcommand_from venv" -l keyring-provider -d 'Attempt to use `keyring` for authentication for index URLs' -r -f -a "{disabled	'Do not use keyring for credential lookup',subprocess	'Use the `keyring` command for credential lookup'}"
complete -c uv -n "__fish_seen_subcommand_from venv" -l exclude-newer -d 'Limit candidate packages to those that were uploaded prior to the given date' -r
complete -c uv -n "__fish_seen_subcommand_from venv" -l link-mode -d 'The method to use when installing packages from the global cache' -r -f -a "{clone	'Clone (i.e., copy-on-write) packages from the wheel into the site packages',copy	'Copy packages from the wheel into the site packages',hardlink	'Hard link packages from the wheel into the site packages'}"
complete -c uv -n "__fish_seen_subcommand_from venv" -l color -d 'Control colors in output' -r -f -a "{auto	'Enables colored output only when the output is going to a terminal or TTY with support',always	'Enables colored output regardless of the detected environment',never	'Disables colored output'}"
complete -c uv -n "__fish_seen_subcommand_from venv" -l cache-dir -d 'Path to the cache directory' -r -F
complete -c uv -n "__fish_seen_subcommand_from venv" -l config-file -d 'The path to a `uv.toml` file to use for configuration' -r -F
complete -c uv -n "__fish_seen_subcommand_from venv" -l system -d 'Use the system Python to uninstall packages'
complete -c uv -n "__fish_seen_subcommand_from venv" -l no-system
complete -c uv -n "__fish_seen_subcommand_from venv" -l seed -d 'Install seed packages (`pip`, `setuptools`, and `wheel`) into the virtual environment'
complete -c uv -n "__fish_seen_subcommand_from venv" -l allow-existing -d 'Preserve any existing files or directories at the target path'
complete -c uv -n "__fish_seen_subcommand_from venv" -l system-site-packages -d 'Give the virtual environment access to the system site packages directory'
complete -c uv -n "__fish_seen_subcommand_from venv" -l no-index -d 'Ignore the registry index (e.g., PyPI), instead relying on direct URL dependencies and those discovered via `--find-links`'
complete -c uv -n "__fish_seen_subcommand_from venv" -l clear
complete -c uv -n "__fish_seen_subcommand_from venv" -l no-seed
complete -c uv -n "__fish_seen_subcommand_from venv" -l no-pip
complete -c uv -n "__fish_seen_subcommand_from venv" -l no-setuptools
complete -c uv -n "__fish_seen_subcommand_from venv" -l no-wheel
complete -c uv -n "__fish_seen_subcommand_from venv" -s q -l quiet -d 'Do not print any output'
complete -c uv -n "__fish_seen_subcommand_from venv" -s v -l verbose -d 'Use verbose output'
complete -c uv -n "__fish_seen_subcommand_from venv" -l no-color -d 'Disable colors; provided for compatibility with `pip`'
complete -c uv -n "__fish_seen_subcommand_from venv" -l native-tls -d 'Whether to load TLS certificates from the platform\'s native certificate store'
complete -c uv -n "__fish_seen_subcommand_from venv" -l no-native-tls
complete -c uv -n "__fish_seen_subcommand_from venv" -l offline -d 'Disable network access, relying only on locally cached data and locally available files'
complete -c uv -n "__fish_seen_subcommand_from venv" -l no-offline
complete -c uv -n "__fish_seen_subcommand_from venv" -l preview -d 'Whether to enable experimental, preview features'
complete -c uv -n "__fish_seen_subcommand_from venv" -l no-preview
complete -c uv -n "__fish_seen_subcommand_from venv" -l isolated -d 'Avoid discovering a `pyproject.toml` or `uv.toml` file in the current directory or any parent directories'
complete -c uv -n "__fish_seen_subcommand_from venv" -l show-settings -d 'Show the resolved settings for the current command'
complete -c uv -n "__fish_seen_subcommand_from venv" -s n -l no-cache -d 'Avoid reading from or writing to the cache'
complete -c uv -n "__fish_seen_subcommand_from venv" -s h -l help -d 'Print help (see more with \'--help\')'
complete -c uv -n "__fish_seen_subcommand_from venv" -s V -l version -d 'Print version'
complete -c uv -n "__fish_seen_subcommand_from cache; and not __fish_seen_subcommand_from clean; and not __fish_seen_subcommand_from prune; and not __fish_seen_subcommand_from dir; and not __fish_seen_subcommand_from help" -l color -d 'Control colors in output' -r -f -a "{auto	'Enables colored output only when the output is going to a terminal or TTY with support',always	'Enables colored output regardless of the detected environment',never	'Disables colored output'}"
complete -c uv -n "__fish_seen_subcommand_from cache; and not __fish_seen_subcommand_from clean; and not __fish_seen_subcommand_from prune; and not __fish_seen_subcommand_from dir; and not __fish_seen_subcommand_from help" -l cache-dir -d 'Path to the cache directory' -r -F
complete -c uv -n "__fish_seen_subcommand_from cache; and not __fish_seen_subcommand_from clean; and not __fish_seen_subcommand_from prune; and not __fish_seen_subcommand_from dir; and not __fish_seen_subcommand_from help" -l config-file -d 'The path to a `uv.toml` file to use for configuration' -r -F
complete -c uv -n "__fish_seen_subcommand_from cache; and not __fish_seen_subcommand_from clean; and not __fish_seen_subcommand_from prune; and not __fish_seen_subcommand_from dir; and not __fish_seen_subcommand_from help" -s q -l quiet -d 'Do not print any output'
complete -c uv -n "__fish_seen_subcommand_from cache; and not __fish_seen_subcommand_from clean; and not __fish_seen_subcommand_from prune; and not __fish_seen_subcommand_from dir; and not __fish_seen_subcommand_from help" -s v -l verbose -d 'Use verbose output'
complete -c uv -n "__fish_seen_subcommand_from cache; and not __fish_seen_subcommand_from clean; and not __fish_seen_subcommand_from prune; and not __fish_seen_subcommand_from dir; and not __fish_seen_subcommand_from help" -l no-color -d 'Disable colors; provided for compatibility with `pip`'
complete -c uv -n "__fish_seen_subcommand_from cache; and not __fish_seen_subcommand_from clean; and not __fish_seen_subcommand_from prune; and not __fish_seen_subcommand_from dir; and not __fish_seen_subcommand_from help" -l native-tls -d 'Whether to load TLS certificates from the platform\'s native certificate store'
complete -c uv -n "__fish_seen_subcommand_from cache; and not __fish_seen_subcommand_from clean; and not __fish_seen_subcommand_from prune; and not __fish_seen_subcommand_from dir; and not __fish_seen_subcommand_from help" -l no-native-tls
complete -c uv -n "__fish_seen_subcommand_from cache; and not __fish_seen_subcommand_from clean; and not __fish_seen_subcommand_from prune; and not __fish_seen_subcommand_from dir; and not __fish_seen_subcommand_from help" -l offline -d 'Disable network access, relying only on locally cached data and locally available files'
complete -c uv -n "__fish_seen_subcommand_from cache; and not __fish_seen_subcommand_from clean; and not __fish_seen_subcommand_from prune; and not __fish_seen_subcommand_from dir; and not __fish_seen_subcommand_from help" -l no-offline
complete -c uv -n "__fish_seen_subcommand_from cache; and not __fish_seen_subcommand_from clean; and not __fish_seen_subcommand_from prune; and not __fish_seen_subcommand_from dir; and not __fish_seen_subcommand_from help" -l preview -d 'Whether to enable experimental, preview features'
complete -c uv -n "__fish_seen_subcommand_from cache; and not __fish_seen_subcommand_from clean; and not __fish_seen_subcommand_from prune; and not __fish_seen_subcommand_from dir; and not __fish_seen_subcommand_from help" -l no-preview
complete -c uv -n "__fish_seen_subcommand_from cache; and not __fish_seen_subcommand_from clean; and not __fish_seen_subcommand_from prune; and not __fish_seen_subcommand_from dir; and not __fish_seen_subcommand_from help" -l isolated -d 'Avoid discovering a `pyproject.toml` or `uv.toml` file in the current directory or any parent directories'
complete -c uv -n "__fish_seen_subcommand_from cache; and not __fish_seen_subcommand_from clean; and not __fish_seen_subcommand_from prune; and not __fish_seen_subcommand_from dir; and not __fish_seen_subcommand_from help" -l show-settings -d 'Show the resolved settings for the current command'
complete -c uv -n "__fish_seen_subcommand_from cache; and not __fish_seen_subcommand_from clean; and not __fish_seen_subcommand_from prune; and not __fish_seen_subcommand_from dir; and not __fish_seen_subcommand_from help" -s n -l no-cache -d 'Avoid reading from or writing to the cache'
complete -c uv -n "__fish_seen_subcommand_from cache; and not __fish_seen_subcommand_from clean; and not __fish_seen_subcommand_from prune; and not __fish_seen_subcommand_from dir; and not __fish_seen_subcommand_from help" -s h -l help -d 'Print help (see more with \'--help\')'
complete -c uv -n "__fish_seen_subcommand_from cache; and not __fish_seen_subcommand_from clean; and not __fish_seen_subcommand_from prune; and not __fish_seen_subcommand_from dir; and not __fish_seen_subcommand_from help" -s V -l version -d 'Print version'
complete -c uv -n "__fish_seen_subcommand_from cache; and not __fish_seen_subcommand_from clean; and not __fish_seen_subcommand_from prune; and not __fish_seen_subcommand_from dir; and not __fish_seen_subcommand_from help" -f -a "clean" -d 'Clear the cache, removing all entries or those linked to specific packages'
complete -c uv -n "__fish_seen_subcommand_from cache; and not __fish_seen_subcommand_from clean; and not __fish_seen_subcommand_from prune; and not __fish_seen_subcommand_from dir; and not __fish_seen_subcommand_from help" -f -a "prune" -d 'Prune all unreachable objects from the cache'
complete -c uv -n "__fish_seen_subcommand_from cache; and not __fish_seen_subcommand_from clean; and not __fish_seen_subcommand_from prune; and not __fish_seen_subcommand_from dir; and not __fish_seen_subcommand_from help" -f -a "dir" -d 'Show the cache directory'
complete -c uv -n "__fish_seen_subcommand_from cache; and not __fish_seen_subcommand_from clean; and not __fish_seen_subcommand_from prune; and not __fish_seen_subcommand_from dir; and not __fish_seen_subcommand_from help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c uv -n "__fish_seen_subcommand_from cache; and __fish_seen_subcommand_from clean" -l color -d 'Control colors in output' -r -f -a "{auto	'Enables colored output only when the output is going to a terminal or TTY with support',always	'Enables colored output regardless of the detected environment',never	'Disables colored output'}"
complete -c uv -n "__fish_seen_subcommand_from cache; and __fish_seen_subcommand_from clean" -l cache-dir -d 'Path to the cache directory' -r -F
complete -c uv -n "__fish_seen_subcommand_from cache; and __fish_seen_subcommand_from clean" -l config-file -d 'The path to a `uv.toml` file to use for configuration' -r -F
complete -c uv -n "__fish_seen_subcommand_from cache; and __fish_seen_subcommand_from clean" -s q -l quiet -d 'Do not print any output'
complete -c uv -n "__fish_seen_subcommand_from cache; and __fish_seen_subcommand_from clean" -s v -l verbose -d 'Use verbose output'
complete -c uv -n "__fish_seen_subcommand_from cache; and __fish_seen_subcommand_from clean" -l no-color -d 'Disable colors; provided for compatibility with `pip`'
complete -c uv -n "__fish_seen_subcommand_from cache; and __fish_seen_subcommand_from clean" -l native-tls -d 'Whether to load TLS certificates from the platform\'s native certificate store'
complete -c uv -n "__fish_seen_subcommand_from cache; and __fish_seen_subcommand_from clean" -l no-native-tls
complete -c uv -n "__fish_seen_subcommand_from cache; and __fish_seen_subcommand_from clean" -l offline -d 'Disable network access, relying only on locally cached data and locally available files'
complete -c uv -n "__fish_seen_subcommand_from cache; and __fish_seen_subcommand_from clean" -l no-offline
complete -c uv -n "__fish_seen_subcommand_from cache; and __fish_seen_subcommand_from clean" -l preview -d 'Whether to enable experimental, preview features'
complete -c uv -n "__fish_seen_subcommand_from cache; and __fish_seen_subcommand_from clean" -l no-preview
complete -c uv -n "__fish_seen_subcommand_from cache; and __fish_seen_subcommand_from clean" -l isolated -d 'Avoid discovering a `pyproject.toml` or `uv.toml` file in the current directory or any parent directories'
complete -c uv -n "__fish_seen_subcommand_from cache; and __fish_seen_subcommand_from clean" -l show-settings -d 'Show the resolved settings for the current command'
complete -c uv -n "__fish_seen_subcommand_from cache; and __fish_seen_subcommand_from clean" -s n -l no-cache -d 'Avoid reading from or writing to the cache'
complete -c uv -n "__fish_seen_subcommand_from cache; and __fish_seen_subcommand_from clean" -s h -l help -d 'Print help (see more with \'--help\')'
complete -c uv -n "__fish_seen_subcommand_from cache; and __fish_seen_subcommand_from clean" -s V -l version -d 'Print version'
complete -c uv -n "__fish_seen_subcommand_from cache; and __fish_seen_subcommand_from prune" -l color -d 'Control colors in output' -r -f -a "{auto	'Enables colored output only when the output is going to a terminal or TTY with support',always	'Enables colored output regardless of the detected environment',never	'Disables colored output'}"
complete -c uv -n "__fish_seen_subcommand_from cache; and __fish_seen_subcommand_from prune" -l cache-dir -d 'Path to the cache directory' -r -F
complete -c uv -n "__fish_seen_subcommand_from cache; and __fish_seen_subcommand_from prune" -l config-file -d 'The path to a `uv.toml` file to use for configuration' -r -F
complete -c uv -n "__fish_seen_subcommand_from cache; and __fish_seen_subcommand_from prune" -s q -l quiet -d 'Do not print any output'
complete -c uv -n "__fish_seen_subcommand_from cache; and __fish_seen_subcommand_from prune" -s v -l verbose -d 'Use verbose output'
complete -c uv -n "__fish_seen_subcommand_from cache; and __fish_seen_subcommand_from prune" -l no-color -d 'Disable colors; provided for compatibility with `pip`'
complete -c uv -n "__fish_seen_subcommand_from cache; and __fish_seen_subcommand_from prune" -l native-tls -d 'Whether to load TLS certificates from the platform\'s native certificate store'
complete -c uv -n "__fish_seen_subcommand_from cache; and __fish_seen_subcommand_from prune" -l no-native-tls
complete -c uv -n "__fish_seen_subcommand_from cache; and __fish_seen_subcommand_from prune" -l offline -d 'Disable network access, relying only on locally cached data and locally available files'
complete -c uv -n "__fish_seen_subcommand_from cache; and __fish_seen_subcommand_from prune" -l no-offline
complete -c uv -n "__fish_seen_subcommand_from cache; and __fish_seen_subcommand_from prune" -l preview -d 'Whether to enable experimental, preview features'
complete -c uv -n "__fish_seen_subcommand_from cache; and __fish_seen_subcommand_from prune" -l no-preview
complete -c uv -n "__fish_seen_subcommand_from cache; and __fish_seen_subcommand_from prune" -l isolated -d 'Avoid discovering a `pyproject.toml` or `uv.toml` file in the current directory or any parent directories'
complete -c uv -n "__fish_seen_subcommand_from cache; and __fish_seen_subcommand_from prune" -l show-settings -d 'Show the resolved settings for the current command'
complete -c uv -n "__fish_seen_subcommand_from cache; and __fish_seen_subcommand_from prune" -s n -l no-cache -d 'Avoid reading from or writing to the cache'
complete -c uv -n "__fish_seen_subcommand_from cache; and __fish_seen_subcommand_from prune" -s h -l help -d 'Print help (see more with \'--help\')'
complete -c uv -n "__fish_seen_subcommand_from cache; and __fish_seen_subcommand_from prune" -s V -l version -d 'Print version'
complete -c uv -n "__fish_seen_subcommand_from cache; and __fish_seen_subcommand_from dir" -l color -d 'Control colors in output' -r -f -a "{auto	'Enables colored output only when the output is going to a terminal or TTY with support',always	'Enables colored output regardless of the detected environment',never	'Disables colored output'}"
complete -c uv -n "__fish_seen_subcommand_from cache; and __fish_seen_subcommand_from dir" -l cache-dir -d 'Path to the cache directory' -r -F
complete -c uv -n "__fish_seen_subcommand_from cache; and __fish_seen_subcommand_from dir" -l config-file -d 'The path to a `uv.toml` file to use for configuration' -r -F
complete -c uv -n "__fish_seen_subcommand_from cache; and __fish_seen_subcommand_from dir" -s q -l quiet -d 'Do not print any output'
complete -c uv -n "__fish_seen_subcommand_from cache; and __fish_seen_subcommand_from dir" -s v -l verbose -d 'Use verbose output'
complete -c uv -n "__fish_seen_subcommand_from cache; and __fish_seen_subcommand_from dir" -l no-color -d 'Disable colors; provided for compatibility with `pip`'
complete -c uv -n "__fish_seen_subcommand_from cache; and __fish_seen_subcommand_from dir" -l native-tls -d 'Whether to load TLS certificates from the platform\'s native certificate store'
complete -c uv -n "__fish_seen_subcommand_from cache; and __fish_seen_subcommand_from dir" -l no-native-tls
complete -c uv -n "__fish_seen_subcommand_from cache; and __fish_seen_subcommand_from dir" -l offline -d 'Disable network access, relying only on locally cached data and locally available files'
complete -c uv -n "__fish_seen_subcommand_from cache; and __fish_seen_subcommand_from dir" -l no-offline
complete -c uv -n "__fish_seen_subcommand_from cache; and __fish_seen_subcommand_from dir" -l preview -d 'Whether to enable experimental, preview features'
complete -c uv -n "__fish_seen_subcommand_from cache; and __fish_seen_subcommand_from dir" -l no-preview
complete -c uv -n "__fish_seen_subcommand_from cache; and __fish_seen_subcommand_from dir" -l isolated -d 'Avoid discovering a `pyproject.toml` or `uv.toml` file in the current directory or any parent directories'
complete -c uv -n "__fish_seen_subcommand_from cache; and __fish_seen_subcommand_from dir" -l show-settings -d 'Show the resolved settings for the current command'
complete -c uv -n "__fish_seen_subcommand_from cache; and __fish_seen_subcommand_from dir" -s n -l no-cache -d 'Avoid reading from or writing to the cache'
complete -c uv -n "__fish_seen_subcommand_from cache; and __fish_seen_subcommand_from dir" -s h -l help -d 'Print help (see more with \'--help\')'
complete -c uv -n "__fish_seen_subcommand_from cache; and __fish_seen_subcommand_from dir" -s V -l version -d 'Print version'
complete -c uv -n "__fish_seen_subcommand_from cache; and __fish_seen_subcommand_from help; and not __fish_seen_subcommand_from clean; and not __fish_seen_subcommand_from prune; and not __fish_seen_subcommand_from dir; and not __fish_seen_subcommand_from help" -f -a "clean" -d 'Clear the cache, removing all entries or those linked to specific packages'
complete -c uv -n "__fish_seen_subcommand_from cache; and __fish_seen_subcommand_from help; and not __fish_seen_subcommand_from clean; and not __fish_seen_subcommand_from prune; and not __fish_seen_subcommand_from dir; and not __fish_seen_subcommand_from help" -f -a "prune" -d 'Prune all unreachable objects from the cache'
complete -c uv -n "__fish_seen_subcommand_from cache; and __fish_seen_subcommand_from help; and not __fish_seen_subcommand_from clean; and not __fish_seen_subcommand_from prune; and not __fish_seen_subcommand_from dir; and not __fish_seen_subcommand_from help" -f -a "dir" -d 'Show the cache directory'
complete -c uv -n "__fish_seen_subcommand_from cache; and __fish_seen_subcommand_from help; and not __fish_seen_subcommand_from clean; and not __fish_seen_subcommand_from prune; and not __fish_seen_subcommand_from dir; and not __fish_seen_subcommand_from help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c uv -n "__fish_seen_subcommand_from self; and not __fish_seen_subcommand_from update; and not __fish_seen_subcommand_from help" -l color -d 'Control colors in output' -r -f -a "{auto	'Enables colored output only when the output is going to a terminal or TTY with support',always	'Enables colored output regardless of the detected environment',never	'Disables colored output'}"
complete -c uv -n "__fish_seen_subcommand_from self; and not __fish_seen_subcommand_from update; and not __fish_seen_subcommand_from help" -l cache-dir -d 'Path to the cache directory' -r -F
complete -c uv -n "__fish_seen_subcommand_from self; and not __fish_seen_subcommand_from update; and not __fish_seen_subcommand_from help" -l config-file -d 'The path to a `uv.toml` file to use for configuration' -r -F
complete -c uv -n "__fish_seen_subcommand_from self; and not __fish_seen_subcommand_from update; and not __fish_seen_subcommand_from help" -s q -l quiet -d 'Do not print any output'
complete -c uv -n "__fish_seen_subcommand_from self; and not __fish_seen_subcommand_from update; and not __fish_seen_subcommand_from help" -s v -l verbose -d 'Use verbose output'
complete -c uv -n "__fish_seen_subcommand_from self; and not __fish_seen_subcommand_from update; and not __fish_seen_subcommand_from help" -l no-color -d 'Disable colors; provided for compatibility with `pip`'
complete -c uv -n "__fish_seen_subcommand_from self; and not __fish_seen_subcommand_from update; and not __fish_seen_subcommand_from help" -l native-tls -d 'Whether to load TLS certificates from the platform\'s native certificate store'
complete -c uv -n "__fish_seen_subcommand_from self; and not __fish_seen_subcommand_from update; and not __fish_seen_subcommand_from help" -l no-native-tls
complete -c uv -n "__fish_seen_subcommand_from self; and not __fish_seen_subcommand_from update; and not __fish_seen_subcommand_from help" -l offline -d 'Disable network access, relying only on locally cached data and locally available files'
complete -c uv -n "__fish_seen_subcommand_from self; and not __fish_seen_subcommand_from update; and not __fish_seen_subcommand_from help" -l no-offline
complete -c uv -n "__fish_seen_subcommand_from self; and not __fish_seen_subcommand_from update; and not __fish_seen_subcommand_from help" -l preview -d 'Whether to enable experimental, preview features'
complete -c uv -n "__fish_seen_subcommand_from self; and not __fish_seen_subcommand_from update; and not __fish_seen_subcommand_from help" -l no-preview
complete -c uv -n "__fish_seen_subcommand_from self; and not __fish_seen_subcommand_from update; and not __fish_seen_subcommand_from help" -l isolated -d 'Avoid discovering a `pyproject.toml` or `uv.toml` file in the current directory or any parent directories'
complete -c uv -n "__fish_seen_subcommand_from self; and not __fish_seen_subcommand_from update; and not __fish_seen_subcommand_from help" -l show-settings -d 'Show the resolved settings for the current command'
complete -c uv -n "__fish_seen_subcommand_from self; and not __fish_seen_subcommand_from update; and not __fish_seen_subcommand_from help" -s n -l no-cache -d 'Avoid reading from or writing to the cache'
complete -c uv -n "__fish_seen_subcommand_from self; and not __fish_seen_subcommand_from update; and not __fish_seen_subcommand_from help" -s h -l help -d 'Print help (see more with \'--help\')'
complete -c uv -n "__fish_seen_subcommand_from self; and not __fish_seen_subcommand_from update; and not __fish_seen_subcommand_from help" -s V -l version -d 'Print version'
complete -c uv -n "__fish_seen_subcommand_from self; and not __fish_seen_subcommand_from update; and not __fish_seen_subcommand_from help" -f -a "update" -d 'Update `uv` to the latest version'
complete -c uv -n "__fish_seen_subcommand_from self; and not __fish_seen_subcommand_from update; and not __fish_seen_subcommand_from help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c uv -n "__fish_seen_subcommand_from self; and __fish_seen_subcommand_from update" -l color -d 'Control colors in output' -r -f -a "{auto	'Enables colored output only when the output is going to a terminal or TTY with support',always	'Enables colored output regardless of the detected environment',never	'Disables colored output'}"
complete -c uv -n "__fish_seen_subcommand_from self; and __fish_seen_subcommand_from update" -l cache-dir -d 'Path to the cache directory' -r -F
complete -c uv -n "__fish_seen_subcommand_from self; and __fish_seen_subcommand_from update" -l config-file -d 'The path to a `uv.toml` file to use for configuration' -r -F
complete -c uv -n "__fish_seen_subcommand_from self; and __fish_seen_subcommand_from update" -s q -l quiet -d 'Do not print any output'
complete -c uv -n "__fish_seen_subcommand_from self; and __fish_seen_subcommand_from update" -s v -l verbose -d 'Use verbose output'
complete -c uv -n "__fish_seen_subcommand_from self; and __fish_seen_subcommand_from update" -l no-color -d 'Disable colors; provided for compatibility with `pip`'
complete -c uv -n "__fish_seen_subcommand_from self; and __fish_seen_subcommand_from update" -l native-tls -d 'Whether to load TLS certificates from the platform\'s native certificate store'
complete -c uv -n "__fish_seen_subcommand_from self; and __fish_seen_subcommand_from update" -l no-native-tls
complete -c uv -n "__fish_seen_subcommand_from self; and __fish_seen_subcommand_from update" -l offline -d 'Disable network access, relying only on locally cached data and locally available files'
complete -c uv -n "__fish_seen_subcommand_from self; and __fish_seen_subcommand_from update" -l no-offline
complete -c uv -n "__fish_seen_subcommand_from self; and __fish_seen_subcommand_from update" -l preview -d 'Whether to enable experimental, preview features'
complete -c uv -n "__fish_seen_subcommand_from self; and __fish_seen_subcommand_from update" -l no-preview
complete -c uv -n "__fish_seen_subcommand_from self; and __fish_seen_subcommand_from update" -l isolated -d 'Avoid discovering a `pyproject.toml` or `uv.toml` file in the current directory or any parent directories'
complete -c uv -n "__fish_seen_subcommand_from self; and __fish_seen_subcommand_from update" -l show-settings -d 'Show the resolved settings for the current command'
complete -c uv -n "__fish_seen_subcommand_from self; and __fish_seen_subcommand_from update" -s n -l no-cache -d 'Avoid reading from or writing to the cache'
complete -c uv -n "__fish_seen_subcommand_from self; and __fish_seen_subcommand_from update" -s h -l help -d 'Print help (see more with \'--help\')'
complete -c uv -n "__fish_seen_subcommand_from self; and __fish_seen_subcommand_from update" -s V -l version -d 'Print version'
complete -c uv -n "__fish_seen_subcommand_from self; and __fish_seen_subcommand_from help; and not __fish_seen_subcommand_from update; and not __fish_seen_subcommand_from help" -f -a "update" -d 'Update `uv` to the latest version'
complete -c uv -n "__fish_seen_subcommand_from self; and __fish_seen_subcommand_from help; and not __fish_seen_subcommand_from update; and not __fish_seen_subcommand_from help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c uv -n "__fish_seen_subcommand_from clean" -l color -d 'Control colors in output' -r -f -a "{auto	'Enables colored output only when the output is going to a terminal or TTY with support',always	'Enables colored output regardless of the detected environment',never	'Disables colored output'}"
complete -c uv -n "__fish_seen_subcommand_from clean" -l cache-dir -d 'Path to the cache directory' -r -F
complete -c uv -n "__fish_seen_subcommand_from clean" -l config-file -d 'The path to a `uv.toml` file to use for configuration' -r -F
complete -c uv -n "__fish_seen_subcommand_from clean" -s q -l quiet -d 'Do not print any output'
complete -c uv -n "__fish_seen_subcommand_from clean" -s v -l verbose -d 'Use verbose output'
complete -c uv -n "__fish_seen_subcommand_from clean" -l no-color -d 'Disable colors; provided for compatibility with `pip`'
complete -c uv -n "__fish_seen_subcommand_from clean" -l native-tls -d 'Whether to load TLS certificates from the platform\'s native certificate store'
complete -c uv -n "__fish_seen_subcommand_from clean" -l no-native-tls
complete -c uv -n "__fish_seen_subcommand_from clean" -l offline -d 'Disable network access, relying only on locally cached data and locally available files'
complete -c uv -n "__fish_seen_subcommand_from clean" -l no-offline
complete -c uv -n "__fish_seen_subcommand_from clean" -l preview -d 'Whether to enable experimental, preview features'
complete -c uv -n "__fish_seen_subcommand_from clean" -l no-preview
complete -c uv -n "__fish_seen_subcommand_from clean" -l isolated -d 'Avoid discovering a `pyproject.toml` or `uv.toml` file in the current directory or any parent directories'
complete -c uv -n "__fish_seen_subcommand_from clean" -l show-settings -d 'Show the resolved settings for the current command'
complete -c uv -n "__fish_seen_subcommand_from clean" -s n -l no-cache -d 'Avoid reading from or writing to the cache'
complete -c uv -n "__fish_seen_subcommand_from clean" -s h -l help -d 'Print help (see more with \'--help\')'
complete -c uv -n "__fish_seen_subcommand_from clean" -s V -l version -d 'Print version'
complete -c uv -n "__fish_seen_subcommand_from version" -l output-format -r -f -a "{text	'',json	''}"
complete -c uv -n "__fish_seen_subcommand_from version" -l color -d 'Control colors in output' -r -f -a "{auto	'Enables colored output only when the output is going to a terminal or TTY with support',always	'Enables colored output regardless of the detected environment',never	'Disables colored output'}"
complete -c uv -n "__fish_seen_subcommand_from version" -l cache-dir -d 'Path to the cache directory' -r -F
complete -c uv -n "__fish_seen_subcommand_from version" -l config-file -d 'The path to a `uv.toml` file to use for configuration' -r -F
complete -c uv -n "__fish_seen_subcommand_from version" -s q -l quiet -d 'Do not print any output'
complete -c uv -n "__fish_seen_subcommand_from version" -s v -l verbose -d 'Use verbose output'
complete -c uv -n "__fish_seen_subcommand_from version" -l no-color -d 'Disable colors; provided for compatibility with `pip`'
complete -c uv -n "__fish_seen_subcommand_from version" -l native-tls -d 'Whether to load TLS certificates from the platform\'s native certificate store'
complete -c uv -n "__fish_seen_subcommand_from version" -l no-native-tls
complete -c uv -n "__fish_seen_subcommand_from version" -l offline -d 'Disable network access, relying only on locally cached data and locally available files'
complete -c uv -n "__fish_seen_subcommand_from version" -l no-offline
complete -c uv -n "__fish_seen_subcommand_from version" -l preview -d 'Whether to enable experimental, preview features'
complete -c uv -n "__fish_seen_subcommand_from version" -l no-preview
complete -c uv -n "__fish_seen_subcommand_from version" -l isolated -d 'Avoid discovering a `pyproject.toml` or `uv.toml` file in the current directory or any parent directories'
complete -c uv -n "__fish_seen_subcommand_from version" -l show-settings -d 'Show the resolved settings for the current command'
complete -c uv -n "__fish_seen_subcommand_from version" -s n -l no-cache -d 'Avoid reading from or writing to the cache'
complete -c uv -n "__fish_seen_subcommand_from version" -s h -l help -d 'Print help (see more with \'--help\')'
complete -c uv -n "__fish_seen_subcommand_from version" -s V -l version -d 'Print version'
complete -c uv -n "__fish_seen_subcommand_from generate-shell-completion" -l color -d 'Control colors in output' -r -f -a "{auto	'Enables colored output only when the output is going to a terminal or TTY with support',always	'Enables colored output regardless of the detected environment',never	'Disables colored output'}"
complete -c uv -n "__fish_seen_subcommand_from generate-shell-completion" -l cache-dir -d 'Path to the cache directory' -r -F
complete -c uv -n "__fish_seen_subcommand_from generate-shell-completion" -l config-file -d 'The path to a `uv.toml` file to use for configuration' -r -F
complete -c uv -n "__fish_seen_subcommand_from generate-shell-completion" -s q -l quiet -d 'Do not print any output'
complete -c uv -n "__fish_seen_subcommand_from generate-shell-completion" -s v -l verbose -d 'Use verbose output'
complete -c uv -n "__fish_seen_subcommand_from generate-shell-completion" -l no-color -d 'Disable colors; provided for compatibility with `pip`'
complete -c uv -n "__fish_seen_subcommand_from generate-shell-completion" -l native-tls -d 'Whether to load TLS certificates from the platform\'s native certificate store'
complete -c uv -n "__fish_seen_subcommand_from generate-shell-completion" -l no-native-tls
complete -c uv -n "__fish_seen_subcommand_from generate-shell-completion" -l offline -d 'Disable network access, relying only on locally cached data and locally available files'
complete -c uv -n "__fish_seen_subcommand_from generate-shell-completion" -l no-offline
complete -c uv -n "__fish_seen_subcommand_from generate-shell-completion" -l preview -d 'Whether to enable experimental, preview features'
complete -c uv -n "__fish_seen_subcommand_from generate-shell-completion" -l no-preview
complete -c uv -n "__fish_seen_subcommand_from generate-shell-completion" -l isolated -d 'Avoid discovering a `pyproject.toml` or `uv.toml` file in the current directory or any parent directories'
complete -c uv -n "__fish_seen_subcommand_from generate-shell-completion" -l show-settings -d 'Show the resolved settings for the current command'
complete -c uv -n "__fish_seen_subcommand_from generate-shell-completion" -s n -l no-cache -d 'Avoid reading from or writing to the cache'
complete -c uv -n "__fish_seen_subcommand_from generate-shell-completion" -s h -l help -d 'Print help (see more with \'--help\')'
complete -c uv -n "__fish_seen_subcommand_from generate-shell-completion" -s V -l version -d 'Print version'
complete -c uv -n "__fish_seen_subcommand_from help; and not __fish_seen_subcommand_from pip; and not __fish_seen_subcommand_from tool; and not __fish_seen_subcommand_from toolchain; and not __fish_seen_subcommand_from run; and not __fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from lock; and not __fish_seen_subcommand_from add; and not __fish_seen_subcommand_from remove; and not __fish_seen_subcommand_from venv; and not __fish_seen_subcommand_from cache; and not __fish_seen_subcommand_from self; and not __fish_seen_subcommand_from clean; and not __fish_seen_subcommand_from version; and not __fish_seen_subcommand_from generate-shell-completion; and not __fish_seen_subcommand_from help" -f -a "pip" -d 'Resolve and install Python packages'
complete -c uv -n "__fish_seen_subcommand_from help; and not __fish_seen_subcommand_from pip; and not __fish_seen_subcommand_from tool; and not __fish_seen_subcommand_from toolchain; and not __fish_seen_subcommand_from run; and not __fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from lock; and not __fish_seen_subcommand_from add; and not __fish_seen_subcommand_from remove; and not __fish_seen_subcommand_from venv; and not __fish_seen_subcommand_from cache; and not __fish_seen_subcommand_from self; and not __fish_seen_subcommand_from clean; and not __fish_seen_subcommand_from version; and not __fish_seen_subcommand_from generate-shell-completion; and not __fish_seen_subcommand_from help" -f -a "tool" -d 'Run and manage executable Python packages'
complete -c uv -n "__fish_seen_subcommand_from help; and not __fish_seen_subcommand_from pip; and not __fish_seen_subcommand_from tool; and not __fish_seen_subcommand_from toolchain; and not __fish_seen_subcommand_from run; and not __fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from lock; and not __fish_seen_subcommand_from add; and not __fish_seen_subcommand_from remove; and not __fish_seen_subcommand_from venv; and not __fish_seen_subcommand_from cache; and not __fish_seen_subcommand_from self; and not __fish_seen_subcommand_from clean; and not __fish_seen_subcommand_from version; and not __fish_seen_subcommand_from generate-shell-completion; and not __fish_seen_subcommand_from help" -f -a "toolchain" -d 'Manage Python installations'
complete -c uv -n "__fish_seen_subcommand_from help; and not __fish_seen_subcommand_from pip; and not __fish_seen_subcommand_from tool; and not __fish_seen_subcommand_from toolchain; and not __fish_seen_subcommand_from run; and not __fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from lock; and not __fish_seen_subcommand_from add; and not __fish_seen_subcommand_from remove; and not __fish_seen_subcommand_from venv; and not __fish_seen_subcommand_from cache; and not __fish_seen_subcommand_from self; and not __fish_seen_subcommand_from clean; and not __fish_seen_subcommand_from version; and not __fish_seen_subcommand_from generate-shell-completion; and not __fish_seen_subcommand_from help" -f -a "run" -d 'Run a command in the project environment'
complete -c uv -n "__fish_seen_subcommand_from help; and not __fish_seen_subcommand_from pip; and not __fish_seen_subcommand_from tool; and not __fish_seen_subcommand_from toolchain; and not __fish_seen_subcommand_from run; and not __fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from lock; and not __fish_seen_subcommand_from add; and not __fish_seen_subcommand_from remove; and not __fish_seen_subcommand_from venv; and not __fish_seen_subcommand_from cache; and not __fish_seen_subcommand_from self; and not __fish_seen_subcommand_from clean; and not __fish_seen_subcommand_from version; and not __fish_seen_subcommand_from generate-shell-completion; and not __fish_seen_subcommand_from help" -f -a "sync" -d 'Sync the project\'s dependencies with the environment'
complete -c uv -n "__fish_seen_subcommand_from help; and not __fish_seen_subcommand_from pip; and not __fish_seen_subcommand_from tool; and not __fish_seen_subcommand_from toolchain; and not __fish_seen_subcommand_from run; and not __fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from lock; and not __fish_seen_subcommand_from add; and not __fish_seen_subcommand_from remove; and not __fish_seen_subcommand_from venv; and not __fish_seen_subcommand_from cache; and not __fish_seen_subcommand_from self; and not __fish_seen_subcommand_from clean; and not __fish_seen_subcommand_from version; and not __fish_seen_subcommand_from generate-shell-completion; and not __fish_seen_subcommand_from help" -f -a "lock" -d 'Resolve the project requirements into a lockfile'
complete -c uv -n "__fish_seen_subcommand_from help; and not __fish_seen_subcommand_from pip; and not __fish_seen_subcommand_from tool; and not __fish_seen_subcommand_from toolchain; and not __fish_seen_subcommand_from run; and not __fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from lock; and not __fish_seen_subcommand_from add; and not __fish_seen_subcommand_from remove; and not __fish_seen_subcommand_from venv; and not __fish_seen_subcommand_from cache; and not __fish_seen_subcommand_from self; and not __fish_seen_subcommand_from clean; and not __fish_seen_subcommand_from version; and not __fish_seen_subcommand_from generate-shell-completion; and not __fish_seen_subcommand_from help" -f -a "add" -d 'Add one or more packages to the project requirements'
complete -c uv -n "__fish_seen_subcommand_from help; and not __fish_seen_subcommand_from pip; and not __fish_seen_subcommand_from tool; and not __fish_seen_subcommand_from toolchain; and not __fish_seen_subcommand_from run; and not __fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from lock; and not __fish_seen_subcommand_from add; and not __fish_seen_subcommand_from remove; and not __fish_seen_subcommand_from venv; and not __fish_seen_subcommand_from cache; and not __fish_seen_subcommand_from self; and not __fish_seen_subcommand_from clean; and not __fish_seen_subcommand_from version; and not __fish_seen_subcommand_from generate-shell-completion; and not __fish_seen_subcommand_from help" -f -a "remove" -d 'Remove one or more packages from the project requirements'
complete -c uv -n "__fish_seen_subcommand_from help; and not __fish_seen_subcommand_from pip; and not __fish_seen_subcommand_from tool; and not __fish_seen_subcommand_from toolchain; and not __fish_seen_subcommand_from run; and not __fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from lock; and not __fish_seen_subcommand_from add; and not __fish_seen_subcommand_from remove; and not __fish_seen_subcommand_from venv; and not __fish_seen_subcommand_from cache; and not __fish_seen_subcommand_from self; and not __fish_seen_subcommand_from clean; and not __fish_seen_subcommand_from version; and not __fish_seen_subcommand_from generate-shell-completion; and not __fish_seen_subcommand_from help" -f -a "venv" -d 'Create a virtual environment'
complete -c uv -n "__fish_seen_subcommand_from help; and not __fish_seen_subcommand_from pip; and not __fish_seen_subcommand_from tool; and not __fish_seen_subcommand_from toolchain; and not __fish_seen_subcommand_from run; and not __fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from lock; and not __fish_seen_subcommand_from add; and not __fish_seen_subcommand_from remove; and not __fish_seen_subcommand_from venv; and not __fish_seen_subcommand_from cache; and not __fish_seen_subcommand_from self; and not __fish_seen_subcommand_from clean; and not __fish_seen_subcommand_from version; and not __fish_seen_subcommand_from generate-shell-completion; and not __fish_seen_subcommand_from help" -f -a "cache" -d 'Manage the cache'
complete -c uv -n "__fish_seen_subcommand_from help; and not __fish_seen_subcommand_from pip; and not __fish_seen_subcommand_from tool; and not __fish_seen_subcommand_from toolchain; and not __fish_seen_subcommand_from run; and not __fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from lock; and not __fish_seen_subcommand_from add; and not __fish_seen_subcommand_from remove; and not __fish_seen_subcommand_from venv; and not __fish_seen_subcommand_from cache; and not __fish_seen_subcommand_from self; and not __fish_seen_subcommand_from clean; and not __fish_seen_subcommand_from version; and not __fish_seen_subcommand_from generate-shell-completion; and not __fish_seen_subcommand_from help" -f -a "self" -d 'Manage the `uv` executable'
complete -c uv -n "__fish_seen_subcommand_from help; and not __fish_seen_subcommand_from pip; and not __fish_seen_subcommand_from tool; and not __fish_seen_subcommand_from toolchain; and not __fish_seen_subcommand_from run; and not __fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from lock; and not __fish_seen_subcommand_from add; and not __fish_seen_subcommand_from remove; and not __fish_seen_subcommand_from venv; and not __fish_seen_subcommand_from cache; and not __fish_seen_subcommand_from self; and not __fish_seen_subcommand_from clean; and not __fish_seen_subcommand_from version; and not __fish_seen_subcommand_from generate-shell-completion; and not __fish_seen_subcommand_from help" -f -a "clean" -d 'Clear the cache, removing all entries or those linked to specific packages'
complete -c uv -n "__fish_seen_subcommand_from help; and not __fish_seen_subcommand_from pip; and not __fish_seen_subcommand_from tool; and not __fish_seen_subcommand_from toolchain; and not __fish_seen_subcommand_from run; and not __fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from lock; and not __fish_seen_subcommand_from add; and not __fish_seen_subcommand_from remove; and not __fish_seen_subcommand_from venv; and not __fish_seen_subcommand_from cache; and not __fish_seen_subcommand_from self; and not __fish_seen_subcommand_from clean; and not __fish_seen_subcommand_from version; and not __fish_seen_subcommand_from generate-shell-completion; and not __fish_seen_subcommand_from help" -f -a "version" -d 'Display uv\'s version'
complete -c uv -n "__fish_seen_subcommand_from help; and not __fish_seen_subcommand_from pip; and not __fish_seen_subcommand_from tool; and not __fish_seen_subcommand_from toolchain; and not __fish_seen_subcommand_from run; and not __fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from lock; and not __fish_seen_subcommand_from add; and not __fish_seen_subcommand_from remove; and not __fish_seen_subcommand_from venv; and not __fish_seen_subcommand_from cache; and not __fish_seen_subcommand_from self; and not __fish_seen_subcommand_from clean; and not __fish_seen_subcommand_from version; and not __fish_seen_subcommand_from generate-shell-completion; and not __fish_seen_subcommand_from help" -f -a "generate-shell-completion" -d 'Generate shell completion'
complete -c uv -n "__fish_seen_subcommand_from help; and not __fish_seen_subcommand_from pip; and not __fish_seen_subcommand_from tool; and not __fish_seen_subcommand_from toolchain; and not __fish_seen_subcommand_from run; and not __fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from lock; and not __fish_seen_subcommand_from add; and not __fish_seen_subcommand_from remove; and not __fish_seen_subcommand_from venv; and not __fish_seen_subcommand_from cache; and not __fish_seen_subcommand_from self; and not __fish_seen_subcommand_from clean; and not __fish_seen_subcommand_from version; and not __fish_seen_subcommand_from generate-shell-completion; and not __fish_seen_subcommand_from help" -f -a "help" -d 'Print this message or the help of the given subcommand(s)'
complete -c uv -n "__fish_seen_subcommand_from help; and __fish_seen_subcommand_from pip; and not __fish_seen_subcommand_from compile; and not __fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from uninstall; and not __fish_seen_subcommand_from freeze; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from show; and not __fish_seen_subcommand_from check" -f -a "compile" -d 'Compile a `requirements.in` file to a `requirements.txt` file'
complete -c uv -n "__fish_seen_subcommand_from help; and __fish_seen_subcommand_from pip; and not __fish_seen_subcommand_from compile; and not __fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from uninstall; and not __fish_seen_subcommand_from freeze; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from show; and not __fish_seen_subcommand_from check" -f -a "sync" -d 'Sync dependencies from a `requirements.txt` file'
complete -c uv -n "__fish_seen_subcommand_from help; and __fish_seen_subcommand_from pip; and not __fish_seen_subcommand_from compile; and not __fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from uninstall; and not __fish_seen_subcommand_from freeze; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from show; and not __fish_seen_subcommand_from check" -f -a "install" -d 'Install packages into the current environment'
complete -c uv -n "__fish_seen_subcommand_from help; and __fish_seen_subcommand_from pip; and not __fish_seen_subcommand_from compile; and not __fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from uninstall; and not __fish_seen_subcommand_from freeze; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from show; and not __fish_seen_subcommand_from check" -f -a "uninstall" -d 'Uninstall packages from the current environment'
complete -c uv -n "__fish_seen_subcommand_from help; and __fish_seen_subcommand_from pip; and not __fish_seen_subcommand_from compile; and not __fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from uninstall; and not __fish_seen_subcommand_from freeze; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from show; and not __fish_seen_subcommand_from check" -f -a "freeze" -d 'Enumerate the installed packages in the current environment'
complete -c uv -n "__fish_seen_subcommand_from help; and __fish_seen_subcommand_from pip; and not __fish_seen_subcommand_from compile; and not __fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from uninstall; and not __fish_seen_subcommand_from freeze; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from show; and not __fish_seen_subcommand_from check" -f -a "list" -d 'Enumerate the installed packages in the current environment'
complete -c uv -n "__fish_seen_subcommand_from help; and __fish_seen_subcommand_from pip; and not __fish_seen_subcommand_from compile; and not __fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from uninstall; and not __fish_seen_subcommand_from freeze; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from show; and not __fish_seen_subcommand_from check" -f -a "show" -d 'Show information about one or more installed packages'
complete -c uv -n "__fish_seen_subcommand_from help; and __fish_seen_subcommand_from pip; and not __fish_seen_subcommand_from compile; and not __fish_seen_subcommand_from sync; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from uninstall; and not __fish_seen_subcommand_from freeze; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from show; and not __fish_seen_subcommand_from check" -f -a "check" -d 'Verify installed packages have compatible dependencies'
complete -c uv -n "__fish_seen_subcommand_from help; and __fish_seen_subcommand_from tool; and not __fish_seen_subcommand_from run" -f -a "run" -d 'Run a tool'
complete -c uv -n "__fish_seen_subcommand_from help; and __fish_seen_subcommand_from toolchain; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from find" -f -a "list" -d 'List the available toolchains'
complete -c uv -n "__fish_seen_subcommand_from help; and __fish_seen_subcommand_from toolchain; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from find" -f -a "install" -d 'Download and install a specific toolchain'
complete -c uv -n "__fish_seen_subcommand_from help; and __fish_seen_subcommand_from toolchain; and not __fish_seen_subcommand_from list; and not __fish_seen_subcommand_from install; and not __fish_seen_subcommand_from find" -f -a "find" -d 'Search for a toolchain'
complete -c uv -n "__fish_seen_subcommand_from help; and __fish_seen_subcommand_from cache; and not __fish_seen_subcommand_from clean; and not __fish_seen_subcommand_from prune; and not __fish_seen_subcommand_from dir" -f -a "clean" -d 'Clear the cache, removing all entries or those linked to specific packages'
complete -c uv -n "__fish_seen_subcommand_from help; and __fish_seen_subcommand_from cache; and not __fish_seen_subcommand_from clean; and not __fish_seen_subcommand_from prune; and not __fish_seen_subcommand_from dir" -f -a "prune" -d 'Prune all unreachable objects from the cache'
complete -c uv -n "__fish_seen_subcommand_from help; and __fish_seen_subcommand_from cache; and not __fish_seen_subcommand_from clean; and not __fish_seen_subcommand_from prune; and not __fish_seen_subcommand_from dir" -f -a "dir" -d 'Show the cache directory'
complete -c uv -n "__fish_seen_subcommand_from help; and __fish_seen_subcommand_from self; and not __fish_seen_subcommand_from update" -f -a "update" -d 'Update `uv` to the latest version'
