# Copyright (c) Microsoft Corporation. All rights reserved.
_copilot_config_context() {
  cfg_mode=read cfg_scope=user cfg_key='' cfg_position=0 cfg_literal=0
  local cfg_stage=root cfg_skip='' cfg_word
  for cfg_word in "$@"; do
    if [ "$cfg_skip" = required ]; then
      cfg_skip=''
      continue
    fi
    if [ "$cfg_skip" = optional ]; then
      cfg_skip=''
      case "$cfg_word" in -*) ;; *) continue ;; esac
    fi
    case "$cfg_stage" in
    root)
      case "$cfg_word" in
      --interactive | -i | --prompt | -p | --model | --reasoning-effort | --context | --auto-tier | --agent | --name | -n | --session-id | --new-session-id | -C | --log-dir | --extension-sdk-path | --config-dir | --log-level | --save-trajectory-output | --prefer-version | --stream | --output-format | --add-dir | --attachment | --disable-mcp-server | --add-github-mcp-toolset | --add-github-mcp-tool | --plugin-dir | --additional-mcp-config | --mcp-github-auth | --additional-content-exclusion-policies | --max-autopilot-continues | --mode | --host | --port | --session-idle-timeout | --auth-token-env | --collect-debug-logs | --collect-debug-logs-output | --ahp-authenticate | --environment | --host-environment | --host-compute-id | --host-ahp-token | --host-ahp-listen | --host-directory | --dynamic-retrieval | --enable-mcp-server | --max-ai-credits | --usage-output-file) cfg_skip=required ;;
      --resume | -r | --connect | --worktree | -w | --share | --allow-tool | --deny-tool | --available-tools | --excluded-tools | --secret-env-vars | --allow-url | --deny-url | --bash-env | --mouse | --sync-session | --ahp) cfg_skip=optional ;;
      --interactive=* | -i?* | --prompt=* | -p?* | --model=* | --reasoning-effort=* | --context=* | --auto-tier=* | --agent=* | --resume=* | -r?* | --name=* | -n?* | --session-id=* | --new-session-id=* | --connect=* | --worktree=* | -w?* | -C?* | --log-dir=* | --extension-sdk-path=* | --config-dir=* | --log-level=* | --save-trajectory-output=* | --prefer-version=* | --stream=* | --output-format=* | --share=* | --add-dir=* | --attachment=* | --disable-mcp-server=* | --add-github-mcp-toolset=* | --add-github-mcp-tool=* | --plugin-dir=* | --additional-mcp-config=* | --mcp-github-auth=* | --additional-content-exclusion-policies=* | --allow-tool=* | --deny-tool=* | --available-tools=* | --excluded-tools=* | --secret-env-vars=* | --allow-url=* | --deny-url=* | --max-autopilot-continues=* | --mode=* | --bash-env=* | --mouse=* | --host=* | --sync-session=* | --port=* | --session-idle-timeout=* | --auth-token-env=* | --collect-debug-logs=* | --collect-debug-logs-output=* | --ahp=* | --ahp-authenticate=* | --environment=* | --host-environment=* | --host-compute-id=* | --host-ahp-token=* | --host-ahp-listen=* | --host-directory=* | --dynamic-retrieval=* | --enable-mcp-server=* | --max-ai-credits=* | --usage-output-file=* | --version | -v | --fleet | --trust-folder | --silent | -s | --disable-client-telemetry | --enable-memory | --enable-reasoning-summaries | --continue | --cloud | --allow-all-tools | --allow-all-paths | --disallow-temp-dir | --banner | --screen-reader | --plain-diff | --share-gist | --disable-builtin-mcps | --enable-all-github-mcp-tools | --allow-all-mcp-server-instructions | --allow-all-urls | --allow-all | --yolo | --autopilot | --plan | --experimental | --show-timing | --server | --ui-server | --headless | --managed-server | --acp | --stdio | --disable-remote-sessions | --remote | --log-interactive-shells | --print-debug-info | --host-ahp | --host-ahp-disable-token | --sandbox | --assisted-approval | --embedded-host | --no-custom-instructions | --no-auto-update | --no-ask-user | --no-color | --no-experimental | --no-bash-env | --no-mouse | --no-auto-login | --no-sandbox | --no-eager-powershell-resolution | --help | -h) ;;
      config) cfg_stage=args ;;
      *) return 1 ;;
      esac
      ;;
    args)
      if [ "$cfg_literal" = 0 ]; then
        case "$cfg_word" in
        --repo | --local)
          cfg_scope=repo
          continue
          ;;
        --global)
          cfg_scope=user
          continue
          ;;
        --rm)
          cfg_mode=remove
          continue
          ;;
        --list)
          cfg_mode=list
          continue
          ;;
        --json) continue ;;
        --)
          cfg_literal=1
          continue
          ;;
        -*) return 1 ;;
        esac
      fi
      if [ "$cfg_position" = 0 ]; then cfg_key="$cfg_word"; fi
      cfg_position=$((cfg_position + 1))
      ;;
    esac
  done
  [ "$cfg_stage" = args ] || return 1
  if [ "$cfg_mode" = read ] && [ "$cfg_position" -gt 0 ]; then cfg_mode=write; fi
  return 0
}

_copilot_config_candidates() {
  cfg_values='' cfg_path='' cfg_list=''
  if [ "$cfg_position" = 0 ]; then
    case "$cfg_scope:$cfg_mode" in
    user:remove) cfg_values='ahpEnvironment.acceptRemoteConnections ahpEnvironment.allowCreatingNewSessions ahpEnvironment.computeId ahpEnvironment.name ahpEnvironment.publishNewSessionsByDefault ahpEnvironment.requireConnectionBinding allowedUrls askUser assistedApprovalInstructions autoTier autoUpdate autoUpdatesChannel banner bannerStyle bashEnv beep beepOnSchedule commandHistoryMaxSize compactPaste companyAnnouncements connectors contextManagementTools contextTier continueOnAutoMode copyOnCodeBlockClick copyOnSelect customAgents.defaultLocalOnly defaultMode defaultPermissionMode deniedUrls disableAllHooks disabledHooks disabledMcpServers disabledSkills dynamicRetrieval.mcp dynamicRetrieval.skills editorMode effortLevel enableAllGithubMcpTools enabledMcpServers experimental extensions.disabledExtensions extensions.mode feature_flags.enabled footer.showAgent footer.showAiUsed footer.showBranch footer.showCiStatus footer.showCodeChanges footer.showContextWindow footer.showCustom footer.showDirectory footer.showModelEffort footer.showPullRequest footer.showQuota footer.showSandbox footer.showSchedules footer.showUsername footer.showYolo githubMcpInsiders githubMcpTools githubMcpToolsets ide.autoConnect ide.openDiffOnEdit ignoredSkillsLocations imageGeneration imageGenerationWatermark includeCoAuthoredBy inlineImageLiveWindow inlineImages keepAlive memory mergeStrategy model modelProviderId mouse notifications pinnedPrompts planContextTier planEffortLevel planModel planModelProviderId powershellFlags proxyKerberosServicePrincipal proxyUrl remoteExport renderHexColors renderMarkdown respectGitignore sandbox.addCurrentWorkingDirectory sandbox.allowBypass sandbox.allowDevToolAccess sandbox.auth.gh sandbox.auth.git sandbox.credentials.envVars.*.injectHosts sandbox.enabled sandbox.sandboxLspServers sandbox.sandboxMcpServers sandbox.userPolicy.filesystem.deniedPaths sandbox.userPolicy.filesystem.readonlyPaths sandbox.userPolicy.filesystem.readwritePaths sandbox.userPolicy.network.allowedHosts sandbox.userPolicy.network.allowLocalNetwork sandbox.userPolicy.network.allowOutbound sandbox.userPolicy.network.blockedHosts sandbox.userPolicy.network.proxy.url sandbox.userPolicy.network.proxy.username screenReader scrollbar showReasoning showTimestamps showTipsOnStartup showToolDurations sidebar.accentActiveSession sidebar.coloredHints sidebar.enabled sidebar.hoverFocus sidebar.showCloseButton sidebar.showHeaderButtons sidebar.showResumableSessions skillDirectories statusLine.padding statusLine.refreshInterval statusLine.type stayInAutopilot stream subagents.contextManagementTools subagents.maxConcurrency subagents.maxDepth tabs.enabled tabs.hide tabs.sort taskbarPresence terminalNotifications terminalProgress theme toolSearch transcriptView updateTerminalTitle voice.selectedDevice.name voice.selectedDevice.occurrence workflows.logPhaseNames workflows.maxConcurrentRuns worktreeBaseRef worktreePathTemplate' ;;
    repo:remove) cfg_values='autoTier companyAnnouncements contextTier deniedUrls disableAllHooks disabledMcpServers disabledSkills effortLevel includeCoAuthoredBy mergeStrategy model modelProviderId respectGitignore skillDirectories' ;;
    *:read) cfg_values='ahpEnvironment.acceptRemoteConnections ahpEnvironment.allowCreatingNewSessions ahpEnvironment.computeId ahpEnvironment.name ahpEnvironment.publishNewSessionsByDefault ahpEnvironment.requireConnectionBinding allowedUrls askUser assistedApprovalInstructions autoTier autoUpdate autoUpdatesChannel banner bannerStyle bashEnv beep beepOnSchedule builtInAgents.rubberDuck builtInAgents.rubberDuckAutoInvoke commandHistoryMaxSize compactPaste companyAnnouncements connectors contextManagementTools contextTier continueOnAutoMode copilotUrl copyOnCodeBlockClick copyOnSelect customAgents.defaultLocalOnly defaultMode defaultPermissionMode deniedUrls disableAllHooks disabledHooks disabledMcpServers disabledSkills dynamicRetrieval.mcp dynamicRetrieval.skills editorMode effortLevel enableAllGithubMcpTools enabledMcpServers experimental extensions.disabledExtensions extensions.mode feature_flags.enabled footer.showAgent footer.showAiUsed footer.showBranch footer.showCiStatus footer.showCodeChanges footer.showContextWindow footer.showCustom footer.showDirectory footer.showModelEffort footer.showPullRequest footer.showQuota footer.showSandbox footer.showSchedules footer.showUsername footer.showYolo githubMcpInsiders githubMcpTools githubMcpToolsets ide.autoConnect ide.openDiffOnEdit ignoredSkillsLocations imageGeneration imageGenerationWatermark includeCoAuthoredBy inlineImageLiveWindow inlineImages keepAlive logLevel memory mergeStrategy model modelProviderId mouse notifications permissions.disableAssistedPermissionsMode permissions.disableBypassPermissionsMode pinnedPrompts planContextTier planEffortLevel planModel planModelProviderId powershellFlags proxyKerberosServicePrincipal proxyUrl remoteExport remoteSessions renderHexColors renderMarkdown respectGitignore sandbox.addCurrentWorkingDirectory sandbox.allowBypass sandbox.allowDevToolAccess sandbox.auth.gh sandbox.auth.git sandbox.credentials.envVars.*.injectHosts sandbox.enabled sandbox.sandboxLspServers sandbox.sandboxMcpServers sandbox.userPolicy.experimental.seatbelt.keychainAccess sandbox.userPolicy.filesystem.deniedPaths sandbox.userPolicy.filesystem.readonlyPaths sandbox.userPolicy.filesystem.readwritePaths sandbox.userPolicy.network.allowedHosts sandbox.userPolicy.network.allowLocalNetwork sandbox.userPolicy.network.allowOutbound sandbox.userPolicy.network.blockedHosts sandbox.userPolicy.network.proxy.password sandbox.userPolicy.network.proxy.url sandbox.userPolicy.network.proxy.username sandbox.userPolicy.seatbelt.keychainAccess screenReader scrollbar shellShortcut showReasoning showTimestamps showTipsOnStartup showToolDurations sidebar.accentActiveSession sidebar.coloredHints sidebar.enabled sidebar.hoverFocus sidebar.showCloseButton sidebar.showHeaderButtons sidebar.showResumableSessions skillDirectories statusLine.command statusLine.padding statusLine.refreshInterval statusLine.type stayInAutopilot storeTokenPlaintext stream streamerMode subagents.contextManagementTools subagents.disabledSubagents subagents.maxConcurrency subagents.maxDepth tabs.enabled tabs.hide tabs.sort taskbarPresence telemetry.capture.identity telemetry.capture.policyDetail telemetry.capture.prompts telemetry.capture.responses telemetry.capture.toolArguments telemetry.capture.toolOutput telemetry.captureContent terminalNotifications terminalProgress theme toolSearch transcriptView updateTerminalTitle voice.enabled voice.selectedDevice.name voice.selectedDevice.occurrence voice.selectedModel workflows.defaultLimits.maxAiCredits workflows.defaultLimits.maxConcurrentSubagents workflows.defaultLimits.maxTotalSubagents workflows.defaultLimits.timeoutSeconds workflows.logPhaseNames workflows.maxConcurrentRuns worktreeBaseRef worktreePathTemplate' ;;
    esac
    return
  fi
  [ "$cfg_position" = 1 ] || return
  [ "$cfg_mode" != list ] || return
  case "$cfg_scope:$cfg_key" in
  'user:ahpEnvironment.acceptRemoteConnections') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:ahpEnvironment.allowCreatingNewSessions') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:ahpEnvironment.computeId') cfg_values='' cfg_path='' cfg_list='' ;;
  'user:ahpEnvironment.name') cfg_values='' cfg_path='' cfg_list='' ;;
  'user:ahpEnvironment.publishNewSessionsByDefault') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:ahpEnvironment.requireConnectionBinding') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:allowedUrls') cfg_values='' cfg_path='' cfg_list='yes' ;;
  'user:askUser') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:assistedApprovalInstructions') cfg_values='' cfg_path='' cfg_list='' ;;
  'user:autoTier' | 'repo:autoTier') cfg_values='efficiency balance intelligence' cfg_path='' cfg_list='' ;;
  'user:autoUpdate') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:autoUpdatesChannel') cfg_values='stable prerelease' cfg_path='' cfg_list='' ;;
  'user:banner') cfg_values='always once never' cfg_path='' cfg_list='' ;;
  'user:bannerStyle') cfg_values='mona classic' cfg_path='' cfg_list='' ;;
  'user:bashEnv') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:beep') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:beepOnSchedule') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:commandHistoryMaxSize') cfg_values='' cfg_path='' cfg_list='' ;;
  'user:compactPaste') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:companyAnnouncements' | 'repo:companyAnnouncements') cfg_values='' cfg_path='' cfg_list='yes' ;;
  'user:connectors') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:contextManagementTools') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:contextTier' | 'repo:contextTier') cfg_values='default long_context' cfg_path='' cfg_list='' ;;
  'user:continueOnAutoMode') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:copyOnCodeBlockClick') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:copyOnSelect') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:customAgents.defaultLocalOnly') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:defaultMode') cfg_values='interactive plan autopilot' cfg_path='' cfg_list='' ;;
  'user:defaultPermissionMode') cfg_values='manual assisted allow-all' cfg_path='' cfg_list='' ;;
  'user:deniedUrls' | 'repo:deniedUrls') cfg_values='' cfg_path='' cfg_list='yes' ;;
  'user:disableAllHooks' | 'repo:disableAllHooks') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:disabledHooks') cfg_values='' cfg_path='' cfg_list='yes' ;;
  'user:disabledMcpServers' | 'repo:disabledMcpServers') cfg_values='' cfg_path='' cfg_list='yes' ;;
  'user:disabledSkills' | 'repo:disabledSkills') cfg_values='' cfg_path='' cfg_list='yes' ;;
  'user:dynamicRetrieval.mcp') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:dynamicRetrieval.skills') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:editorMode') cfg_values='normal vim' cfg_path='' cfg_list='' ;;
  'user:effortLevel' | 'repo:effortLevel') cfg_values='' cfg_path='' cfg_list='' ;;
  'user:enableAllGithubMcpTools') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:enabledMcpServers') cfg_values='' cfg_path='' cfg_list='yes' ;;
  'user:experimental') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:extensions.disabledExtensions') cfg_values='' cfg_path='' cfg_list='yes' ;;
  'user:extensions.mode') cfg_values='disabled load_only load_and_augment' cfg_path='' cfg_list='' ;;
  'user:feature_flags.enabled') cfg_values='' cfg_path='' cfg_list='yes' ;;
  'user:footer.showAgent') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:footer.showAiUsed') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:footer.showBranch') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:footer.showCiStatus') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:footer.showCodeChanges') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:footer.showContextWindow') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:footer.showCustom') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:footer.showDirectory') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:footer.showModelEffort') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:footer.showPullRequest') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:footer.showQuota') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:footer.showSandbox') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:footer.showSchedules') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:footer.showUsername') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:footer.showYolo') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:githubMcpInsiders') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:githubMcpTools') cfg_values='' cfg_path='' cfg_list='yes' ;;
  'user:githubMcpToolsets') cfg_values='' cfg_path='' cfg_list='yes' ;;
  'user:ide.autoConnect') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:ide.openDiffOnEdit') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:ignoredSkillsLocations') cfg_values='' cfg_path='directory' cfg_list='yes' ;;
  'user:imageGeneration') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:imageGenerationWatermark') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:includeCoAuthoredBy' | 'repo:includeCoAuthoredBy') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:inlineImageLiveWindow') cfg_values='' cfg_path='' cfg_list='' ;;
  'user:inlineImages') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:keepAlive') cfg_values='on off busy' cfg_path='' cfg_list='' ;;
  'user:memory') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:mergeStrategy' | 'repo:mergeStrategy') cfg_values='rebase merge' cfg_path='' cfg_list='' ;;
  'user:model' | 'repo:model') cfg_values='' cfg_path='' cfg_list='' ;;
  'user:modelProviderId' | 'repo:modelProviderId') cfg_values='' cfg_path='' cfg_list='' ;;
  'user:mouse') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:notifications') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:pinnedPrompts') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:planContextTier') cfg_values='default long_context' cfg_path='' cfg_list='' ;;
  'user:planEffortLevel') cfg_values='' cfg_path='' cfg_list='' ;;
  'user:planModel') cfg_values='' cfg_path='' cfg_list='' ;;
  'user:planModelProviderId') cfg_values='' cfg_path='' cfg_list='' ;;
  'user:powershellFlags') cfg_values='' cfg_path='' cfg_list='yes' ;;
  'user:proxyKerberosServicePrincipal') cfg_values='' cfg_path='' cfg_list='' ;;
  'user:proxyUrl') cfg_values='' cfg_path='' cfg_list='' ;;
  'user:remoteExport') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:renderHexColors') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:renderMarkdown') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:respectGitignore' | 'repo:respectGitignore') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:sandbox.addCurrentWorkingDirectory') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:sandbox.allowBypass') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:sandbox.allowDevToolAccess') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:sandbox.auth.gh') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:sandbox.auth.git') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:sandbox.credentials.envVars.*.injectHosts') cfg_values='' cfg_path='' cfg_list='yes' ;;
  'user:sandbox.enabled') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:sandbox.sandboxLspServers') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:sandbox.sandboxMcpServers') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:sandbox.userPolicy.filesystem.deniedPaths') cfg_values='' cfg_path='file' cfg_list='yes' ;;
  'user:sandbox.userPolicy.filesystem.readonlyPaths') cfg_values='' cfg_path='file' cfg_list='yes' ;;
  'user:sandbox.userPolicy.filesystem.readwritePaths') cfg_values='' cfg_path='file' cfg_list='yes' ;;
  'user:sandbox.userPolicy.network.allowedHosts') cfg_values='' cfg_path='' cfg_list='yes' ;;
  'user:sandbox.userPolicy.network.allowLocalNetwork') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:sandbox.userPolicy.network.allowOutbound') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:sandbox.userPolicy.network.blockedHosts') cfg_values='' cfg_path='' cfg_list='yes' ;;
  'user:sandbox.userPolicy.network.proxy.url') cfg_values='' cfg_path='' cfg_list='' ;;
  'user:sandbox.userPolicy.network.proxy.username') cfg_values='' cfg_path='' cfg_list='' ;;
  'user:screenReader') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:scrollbar') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:showReasoning') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:showTimestamps') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:showTipsOnStartup') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:showToolDurations') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:sidebar.accentActiveSession') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:sidebar.coloredHints') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:sidebar.enabled') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:sidebar.hoverFocus') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:sidebar.showCloseButton') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:sidebar.showHeaderButtons') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:sidebar.showResumableSessions') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:skillDirectories' | 'repo:skillDirectories') cfg_values='' cfg_path='directory' cfg_list='yes' ;;
  'user:statusLine.padding') cfg_values='' cfg_path='' cfg_list='' ;;
  'user:statusLine.refreshInterval') cfg_values='' cfg_path='' cfg_list='' ;;
  'user:statusLine.type') cfg_values='command' cfg_path='' cfg_list='' ;;
  'user:stayInAutopilot') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:stream') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:subagents.contextManagementTools') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:subagents.maxConcurrency') cfg_values='' cfg_path='' cfg_list='' ;;
  'user:subagents.maxDepth') cfg_values='' cfg_path='' cfg_list='' ;;
  'user:tabs.enabled') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:tabs.hide') cfg_values='' cfg_path='' cfg_list='yes' ;;
  'user:tabs.sort') cfg_values='' cfg_path='' cfg_list='yes' ;;
  'user:taskbarPresence') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:terminalNotifications') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:terminalProgress') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:theme') cfg_values='default github dim high-contrast colorblind' cfg_path='' cfg_list='' ;;
  'user:toolSearch') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:transcriptView') cfg_values='default concise' cfg_path='' cfg_list='' ;;
  'user:updateTerminalTitle') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:voice.selectedDevice.name') cfg_values='' cfg_path='' cfg_list='' ;;
  'user:voice.selectedDevice.occurrence') cfg_values='' cfg_path='' cfg_list='' ;;
  'user:workflows.logPhaseNames') cfg_values='true false' cfg_path='' cfg_list='' ;;
  'user:workflows.maxConcurrentRuns') cfg_values='' cfg_path='' cfg_list='' ;;
  'user:worktreeBaseRef') cfg_values='head defaultBranch' cfg_path='' cfg_list='' ;;
  'user:worktreePathTemplate') cfg_values='' cfg_path='directory' cfg_list='' ;;
  esac
  if [ "$cfg_mode" = remove ] && [ "$cfg_list" != yes ]; then
    cfg_values='' cfg_path=''
  fi
}

_copilot_config_key_candidates() {
  local candidate prefix suffix name
  for candidate in "$@"; do
    case "$candidate" in
    *'.*.'*)
      prefix="${candidate%%\*.*}"
      suffix="${candidate#*.\*}"
      case "$cur" in
      "$prefix"*)
        name="${cur#"$prefix"}"
        case "$name" in
        ?*.*)
          name="${name%.*}"
          candidate="$prefix$name$suffix"
          ;;
        *) continue ;;
        esac
        ;;
      *) candidate="$prefix" ;;
      esac
      ;;
    esac
    case "$candidate" in "$cur"*) printf '%s\n' "$candidate" ;; esac
  done
}

# Copyright (c) Microsoft Corporation. All rights reserved.
_copilot_config_complete() {
  local cfg_mode cfg_scope cfg_key cfg_position cfg_literal cfg_values cfg_path cfg_list
  local cur="${COMP_WORDS[COMP_CWORD]}" candidate
  _copilot_config_context "${COMP_WORDS[@]:1:COMP_CWORD-1}" || return 1
  # Leave option completion to clap, unless -- made this a literal operand.
  if [[ "$cur" == -* && "$cfg_literal" = 0 ]]; then return 1; fi
  _copilot_config_candidates
  COMPREPLY=()
  if [[ "$cfg_position" = 0 && -n "$cfg_values" ]]; then
    local -a candidates
    read -r -a candidates <<<"$cfg_values"
    while IFS= read -r candidate; do
      COMPREPLY+=("$candidate")
      [[ "$candidate" = *. ]] && compopt -o nospace 2>/dev/null || :
    done < <(_copilot_config_key_candidates "${candidates[@]}")
  elif [[ -n "$cfg_values" ]]; then
    while IFS= read -r candidate; do COMPREPLY+=("$candidate"); done < <(compgen -W "$cfg_values" -- "$cur")
  elif [[ -n "$cfg_path" ]]; then
    local search="$cur" action=-f
    [[ "$cfg_path" = directory ]] && action=-d
    [[ "$search" = '~/'* ]] && search="$HOME/${search:2}"
    while IFS= read -r candidate; do
      [[ -d "$candidate" ]] && candidate="$candidate/"
      [[ "$cur" = '~/'* ]] && candidate="~/${candidate#"$HOME/"}"
      COMPREPLY+=("$candidate")
    done < <(compgen "$action" -- "$search")
    compopt -o filenames 2>/dev/null || :
  fi
  # Do not offer unrelated files for boolean, enum, key, or free-text operands.
  compopt +o default +o bashdefault 2>/dev/null || :
  return 0
}

_copilot() {
  _copilot_config_complete && return 0
  local i cur prev opts cmd
  COMPREPLY=()
  if [[ "${BASH_VERSINFO[0]}" -ge 4 ]]; then
    cur="$2"
  else
    cur="${COMP_WORDS[COMP_CWORD]}"
  fi
  prev="$3"
  cmd=""
  opts=""

  for i in "${COMP_WORDS[@]:0:COMP_CWORD}"; do
    case "${cmd},${i}" in
    ",$1")
      cmd="copilot"
      ;;
    copilot,app)
      cmd="copilot__subcmd__app"
      ;;
    copilot,completion)
      cmd="copilot__subcmd__completion"
      ;;
    copilot,config)
      cmd="copilot__subcmd__config"
      ;;
    copilot,help)
      cmd="copilot__subcmd__help"
      ;;
    copilot,init)
      cmd="copilot__subcmd__init"
      ;;
    copilot,instruction)
      cmd="copilot__subcmd__instruction"
      ;;
    copilot,login)
      cmd="copilot__subcmd__login"
      ;;
    copilot,lsp)
      cmd="copilot__subcmd__lsp"
      ;;
    copilot,mcp)
      cmd="copilot__subcmd__mcp"
      ;;
    copilot,memories)
      cmd="copilot__subcmd__memories"
      ;;
    copilot,plugin)
      cmd="copilot__subcmd__plugin"
      ;;
    copilot,sandbox)
      cmd="copilot__subcmd__sandbox"
      ;;
    copilot,sessions)
      cmd="copilot__subcmd__sessions"
      ;;
    copilot,skill)
      cmd="copilot__subcmd__skill"
      ;;
    copilot,taskbar-selftest)
      cmd="copilot__subcmd__taskbar__subcmd__selftest"
      ;;
    copilot,update)
      cmd="copilot__subcmd__update"
      ;;
    copilot,version)
      cmd="copilot__subcmd__version"
      ;;
    copilot,workflow)
      cmd="copilot__subcmd__workflow"
      ;;
    copilot__subcmd__instruction,list)
      cmd="copilot__subcmd__instruction__subcmd__list"
      ;;
    copilot__subcmd__lsp,list)
      cmd="copilot__subcmd__lsp__subcmd__list"
      ;;
    copilot__subcmd__mcp,add)
      cmd="copilot__subcmd__mcp__subcmd__add"
      ;;
    copilot__subcmd__mcp,disable)
      cmd="copilot__subcmd__mcp__subcmd__disable"
      ;;
    copilot__subcmd__mcp,enable)
      cmd="copilot__subcmd__mcp__subcmd__enable"
      ;;
    copilot__subcmd__mcp,get)
      cmd="copilot__subcmd__mcp__subcmd__get"
      ;;
    copilot__subcmd__mcp,list)
      cmd="copilot__subcmd__mcp__subcmd__list"
      ;;
    copilot__subcmd__mcp,remove)
      cmd="copilot__subcmd__mcp__subcmd__remove"
      ;;
    copilot__subcmd__memories,import)
      cmd="copilot__subcmd__memories__subcmd__import"
      ;;
    copilot__subcmd__plugin,disable)
      cmd="copilot__subcmd__plugin__subcmd__disable"
      ;;
    copilot__subcmd__plugin,enable)
      cmd="copilot__subcmd__plugin__subcmd__enable"
      ;;
    copilot__subcmd__plugin,install)
      cmd="copilot__subcmd__plugin__subcmd__install"
      ;;
    copilot__subcmd__plugin,list)
      cmd="copilot__subcmd__plugin__subcmd__list"
      ;;
    copilot__subcmd__plugin,marketplace)
      cmd="copilot__subcmd__plugin__subcmd__marketplace"
      ;;
    copilot__subcmd__plugin,uninstall)
      cmd="copilot__subcmd__plugin__subcmd__uninstall"
      ;;
    copilot__subcmd__plugin,update)
      cmd="copilot__subcmd__plugin__subcmd__update"
      ;;
    copilot__subcmd__plugin__subcmd__marketplace,add)
      cmd="copilot__subcmd__plugin__subcmd__marketplace__subcmd__add"
      ;;
    copilot__subcmd__plugin__subcmd__marketplace,browse)
      cmd="copilot__subcmd__plugin__subcmd__marketplace__subcmd__browse"
      ;;
    copilot__subcmd__plugin__subcmd__marketplace,list)
      cmd="copilot__subcmd__plugin__subcmd__marketplace__subcmd__list"
      ;;
    copilot__subcmd__plugin__subcmd__marketplace,remove)
      cmd="copilot__subcmd__plugin__subcmd__marketplace__subcmd__remove"
      ;;
    copilot__subcmd__plugin__subcmd__marketplace,update)
      cmd="copilot__subcmd__plugin__subcmd__marketplace__subcmd__update"
      ;;
    copilot__subcmd__sandbox,ca)
      cmd="copilot__subcmd__sandbox__subcmd__ca"
      ;;
    copilot__subcmd__sandbox__subcmd__ca,create)
      cmd="copilot__subcmd__sandbox__subcmd__ca__subcmd__create"
      ;;
    copilot__subcmd__sandbox__subcmd__ca,remove)
      cmd="copilot__subcmd__sandbox__subcmd__ca__subcmd__remove"
      ;;
    copilot__subcmd__sandbox__subcmd__ca,rotate)
      cmd="copilot__subcmd__sandbox__subcmd__ca__subcmd__rotate"
      ;;
    copilot__subcmd__sandbox__subcmd__ca,status)
      cmd="copilot__subcmd__sandbox__subcmd__ca__subcmd__status"
      ;;
    copilot__subcmd__sandbox__subcmd__ca,trust)
      cmd="copilot__subcmd__sandbox__subcmd__ca__subcmd__trust"
      ;;
    copilot__subcmd__sessions,import)
      cmd="copilot__subcmd__sessions__subcmd__import"
      ;;
    copilot__subcmd__skill,add)
      cmd="copilot__subcmd__skill__subcmd__add"
      ;;
    copilot__subcmd__skill,disable)
      cmd="copilot__subcmd__skill__subcmd__disable"
      ;;
    copilot__subcmd__skill,enable)
      cmd="copilot__subcmd__skill__subcmd__enable"
      ;;
    copilot__subcmd__skill,list)
      cmd="copilot__subcmd__skill__subcmd__list"
      ;;
    copilot__subcmd__skill,remove)
      cmd="copilot__subcmd__skill__subcmd__remove"
      ;;
    copilot__subcmd__workflow,run)
      cmd="copilot__subcmd__workflow__subcmd__run"
      ;;
    *)
      ;;
    esac
  done

  case "${cmd}" in
  copilot)
    opts="-v -i -p -s -r -n -w -C -h --version --interactive --fleet --prompt --trust-folder --silent --disable-client-telemetry --enable-memory --model --reasoning-effort --context --auto-tier --enable-reasoning-summaries --agent --resume --continue --name --session-id --new-session-id --connect --cloud --worktree --allow-all-tools --allow-all-paths --disallow-temp-dir --banner --screen-reader --plain-diff --log-dir --extension-sdk-path --config-dir --log-level --save-trajectory-output --prefer-version --stream --output-format --share --share-gist --add-dir --attachment --disable-mcp-server --disable-builtin-mcps --enable-all-github-mcp-tools --add-github-mcp-toolset --add-github-mcp-tool --plugin-dir --additional-mcp-config --mcp-github-auth --allow-all-mcp-server-instructions --additional-content-exclusion-policies --allow-tool --deny-tool --available-tools --excluded-tools --secret-env-vars --allow-url --deny-url --allow-all-urls --allow-all --yolo --max-autopilot-continues --mode --autopilot --plan --experimental --bash-env --mouse --show-timing --server --ui-server --headless --managed-server --acp --stdio --host --disable-remote-sessions --sync-session --port --session-idle-timeout --auth-token-env --log-interactive-shells --print-debug-info --collect-debug-logs --collect-debug-logs-output --ahp --ahp-authenticate --environment --host-environment --host-compute-id --host-ahp --host-ahp-token --host-ahp-disable-token --host-ahp-listen --host-directory --sandbox --dynamic-retrieval --enable-mcp-server --max-ai-credits --assisted-approval --embedded-host --usage-output-file --no-custom-instructions --no-auto-update --no-ask-user --no-color --no-experimental --no-bash-env --no-mouse --no-auto-login --no-sandbox --no-eager-powershell-resolution --help app login help init config update version workflow sessions memories plugin mcp skill instruction lsp sandbox taskbar-selftest completion"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 1 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    --interactive)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    -i)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --prompt)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    -p)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --model)
      COMPREPLY=($(compgen -W "auto claude-sonnet-5.5 claude-sonnet-5 claude-fable-5.1 claude-fable-5 claude-opus-5.5 claude-opus-5 claude-opus-4.8 claude-opus-4.8-fast claude-sonnet-4.6 claude-haiku-5.5 claude-haiku-4.5 gpt-6.1-sol gpt-6-sol gpt-6-luna gpt-6-astra gpt-5.6-sol gpt-5.6-terra gpt-5.6-luna gpt-5.5 gpt-5.4 gpt-5.4-mini gpt-5.3-codex gpt-5-mini mai-code-1.1-flash gemini-4-argon gemini-3.8-flash gemini-3.7-flash grok-4.7 grok-4.6 grok-4.5 kimi-k3" -- "${cur}"))
      return 0
      ;;
    --reasoning-effort)
      COMPREPLY=($(compgen -W "none minimal low medium high xhigh max" -- "${cur}"))
      return 0
      ;;
    --context)
      COMPREPLY=($(compgen -W "default long_context" -- "${cur}"))
      return 0
      ;;
    --auto-tier)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --agent)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --resume)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    -r)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --name)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    -n)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --session-id)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --new-session-id)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --connect)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --worktree)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    -w)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    -C)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --log-dir)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --extension-sdk-path)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --config-dir)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --log-level)
      COMPREPLY=($(compgen -W "none error warning info debug all default" -- "${cur}"))
      return 0
      ;;
    --save-trajectory-output)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --prefer-version)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --stream)
      COMPREPLY=($(compgen -W "on off" -- "${cur}"))
      return 0
      ;;
    --output-format)
      COMPREPLY=($(compgen -W "text json" -- "${cur}"))
      return 0
      ;;
    --share)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --add-dir)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --attachment)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --disable-mcp-server)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --add-github-mcp-toolset)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --add-github-mcp-tool)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --plugin-dir)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --additional-mcp-config)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --mcp-github-auth)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --additional-content-exclusion-policies)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --allow-tool)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --deny-tool)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --available-tools)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --excluded-tools)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --secret-env-vars)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --allow-url)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --deny-url)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --max-autopilot-continues)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --mode)
      COMPREPLY=($(compgen -W "interactive plan autopilot" -- "${cur}"))
      return 0
      ;;
    --bash-env)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --mouse)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --host)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --sync-session)
      COMPREPLY=($(compgen -W "on off" -- "${cur}"))
      return 0
      ;;
    --port)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --session-idle-timeout)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --auth-token-env)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --collect-debug-logs)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --collect-debug-logs-output)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --ahp)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --ahp-authenticate)
      COMPREPLY=($(compgen -W "github" -- "${cur}"))
      return 0
      ;;
    --environment)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --host-environment)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --host-compute-id)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --host-ahp-token)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --host-ahp-listen)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --host-directory)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --dynamic-retrieval)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --enable-mcp-server)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --max-ai-credits)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --usage-output-file)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  copilot__subcmd__app)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  copilot__subcmd__completion)
    opts="-h --help bash zsh fish"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  copilot__subcmd__config)
    opts="-h --list --rm --json --global --repo --local --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  copilot__subcmd__help)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  copilot__subcmd__init)
    opts="-h --disable-client-telemetry --sandbox --experimental --no-sandbox --no-experimental --no-eager-powershell-resolution --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  copilot__subcmd__instruction)
    opts="-h --help list"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  copilot__subcmd__instruction__subcmd__list)
    opts="-h --json --config-dir --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    --config-dir)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  copilot__subcmd__login)
    opts="-h --host --device-code --web-flow --with-token --config-dir --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    --host)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --config-dir)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  copilot__subcmd__lsp)
    opts="-h --help list"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  copilot__subcmd__lsp__subcmd__list)
    opts="-h --json --config-dir --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    --config-dir)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  copilot__subcmd__mcp)
    opts="-h --help list get add remove enable disable"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  copilot__subcmd__mcp__subcmd__add)
    opts="-h --transport --env --header --tools --timeout --json --show-secrets --config-dir --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    --transport)
      COMPREPLY=($(compgen -W "stdio http sse" -- "${cur}"))
      return 0
      ;;
    --env)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --header)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --tools)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --timeout)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --config-dir)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  copilot__subcmd__mcp__subcmd__disable)
    opts="-h --config-dir --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    --config-dir)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  copilot__subcmd__mcp__subcmd__enable)
    opts="-h --config-dir --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    --config-dir)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  copilot__subcmd__mcp__subcmd__get)
    opts="-h --json --show-secrets --config-dir --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    --config-dir)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  copilot__subcmd__mcp__subcmd__list)
    opts="-h --json --config-dir --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    --config-dir)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  copilot__subcmd__mcp__subcmd__remove)
    opts="-h --config-dir --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    --config-dir)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  copilot__subcmd__memories)
    opts="-h --help import"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  copilot__subcmd__memories__subcmd__import)
    opts="-h --dry-run --output --on-conflict --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    --output)
      COMPREPLY=($(compgen -W "json" -- "${cur}"))
      return 0
      ;;
    --on-conflict)
      COMPREPLY=($(compgen -W "skip error" -- "${cur}"))
      return 0
      ;;
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  copilot__subcmd__plugin)
    opts="-h --help install uninstall update list enable disable marketplace"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  copilot__subcmd__plugin__subcmd__disable)
    opts="-h --config-dir --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    --config-dir)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  copilot__subcmd__plugin__subcmd__enable)
    opts="-h --config-dir --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    --config-dir)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  copilot__subcmd__plugin__subcmd__install)
    opts="-h --from-github --descriptor-digest --bundle-digest --schema-version --config-dir --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    --from-github)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --descriptor-digest)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --bundle-digest)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --schema-version)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --config-dir)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  copilot__subcmd__plugin__subcmd__list)
    opts="-h --json --config-dir --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    --config-dir)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  copilot__subcmd__plugin__subcmd__marketplace)
    opts="-h --help add remove list browse update"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  copilot__subcmd__plugin__subcmd__marketplace__subcmd__add)
    opts="-h --config-dir --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    --config-dir)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  copilot__subcmd__plugin__subcmd__marketplace__subcmd__browse)
    opts="-h --json --config-dir --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    --config-dir)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  copilot__subcmd__plugin__subcmd__marketplace__subcmd__list)
    opts="-h --json --config-dir --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    --config-dir)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  copilot__subcmd__plugin__subcmd__marketplace__subcmd__remove)
    opts="-f -h --force --config-dir --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    --config-dir)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  copilot__subcmd__plugin__subcmd__marketplace__subcmd__update)
    opts="-h --config-dir --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    --config-dir)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  copilot__subcmd__plugin__subcmd__uninstall)
    opts="-h --config-dir --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    --config-dir)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  copilot__subcmd__plugin__subcmd__update)
    opts="-h --all --config-dir --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    --config-dir)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  copilot__subcmd__sandbox)
    opts="-h --help ca"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  copilot__subcmd__sandbox__subcmd__ca)
    opts="-h --help status rotate remove create trust"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  copilot__subcmd__sandbox__subcmd__ca__subcmd__create)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  copilot__subcmd__sandbox__subcmd__ca__subcmd__remove)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  copilot__subcmd__sandbox__subcmd__ca__subcmd__rotate)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  copilot__subcmd__sandbox__subcmd__ca__subcmd__status)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  copilot__subcmd__sandbox__subcmd__ca__subcmd__trust)
    opts="-h --allow-host --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 4 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    --allow-host)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  copilot__subcmd__sessions)
    opts="-h --help import"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  copilot__subcmd__sessions__subcmd__import)
    opts="-h --dry-run --output --working-directory --name --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    --output)
      COMPREPLY=($(compgen -W "json" -- "${cur}"))
      return 0
      ;;
    --working-directory)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --name)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  copilot__subcmd__skill)
    opts="-h --help list add remove enable disable"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  copilot__subcmd__skill__subcmd__add)
    opts="-h --project --config-dir --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    --config-dir)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  copilot__subcmd__skill__subcmd__disable)
    opts="-h --config-dir --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    --config-dir)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  copilot__subcmd__skill__subcmd__enable)
    opts="-h --config-dir --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    --config-dir)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  copilot__subcmd__skill__subcmd__list)
    opts="-h --json --config-dir --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    --config-dir)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  copilot__subcmd__skill__subcmd__remove)
    opts="-h --config-dir --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    --config-dir)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  copilot__subcmd__taskbar__subcmd__selftest)
    opts="-h --strict --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  copilot__subcmd__update)
    opts="-h --help stable prerelease"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  copilot__subcmd__version)
    opts="-h --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  copilot__subcmd__workflow)
    opts="-h --help run"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 2 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  copilot__subcmd__workflow__subcmd__run)
    opts="-s -h --args --result-file --silent --output-format --help"
    if [[ ${cur} == -* || ${COMP_CWORD} -eq 3 ]]; then
      COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
      return 0
    fi
    case "${prev}" in
    --args)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --result-file)
      COMPREPLY=($(compgen -f "${cur}"))
      return 0
      ;;
    --output-format)
      COMPREPLY=($(compgen -W "text json" -- "${cur}"))
      return 0
      ;;
    *)
      COMPREPLY=()
      ;;
    esac
    COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
    return 0
    ;;
  esac
}

if [[ "${BASH_VERSINFO[0]}" -eq 4 && "${BASH_VERSINFO[1]}" -ge 4 || "${BASH_VERSINFO[0]}" -gt 4 ]]; then
  complete -F _copilot -o nosort -o bashdefault -o default copilot
else
  complete -F _copilot -o bashdefault -o default copilot
fi
