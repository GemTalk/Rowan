"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: initializing - private
"
initializeRepositoryDirectory
  self directoryPath: self url pathForDirectory.
  self ensureDirectoryPathExists
