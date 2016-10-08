"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: running
"
setUp
  | utils |
  super setUp.
  gitRepoDir := CypressFileSystemGitRepository gitRepositoryDir.
  utils := self fileUtils.
  CypressFileSystemGitRepository
    gitRepositoryDir:
      utils workingDirectory , utils pathNameDelimiter , 'cypressTestRepos'
        , utils pathNameDelimiter.
  utils ensureDirectoryExists: CypressFileSystemGitRepository gitRepositoryDir