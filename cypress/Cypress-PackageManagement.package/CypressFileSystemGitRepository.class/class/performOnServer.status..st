"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: git commands
"
performOnServer: commandLine status: statusBlock
  | performOnServerStatusArray |
  performOnServerStatusArray := System _performOnServer: commandLine.
  ^ statusBlock value: performOnServerStatusArray