"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: running
"
tearDown
  | utils |
  super tearDown.
  utils := self fileUtils.
  utils deleteAll: CypressFileSystemGitRepository gitRepositoryDir.
  CypressFileSystemGitRepository gitRepositoryDir: gitRepoDir