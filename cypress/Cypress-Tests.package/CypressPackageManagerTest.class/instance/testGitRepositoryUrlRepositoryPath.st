"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: tests
"
testGitRepositoryUrlRepositoryPath
  "self debug: #testGitRepositoryUrlRepositoryPath"

  {'gitcypress'.
  'gitfiletree'} do: [ :scheme | {{'://github.com/dalehenrich/filetree'.
      ''}.
      {'://github.com/dalehenrich/filetree/repository'.
      'repository'}.
      {'://github.com/dalehenrich/filetree:pharo5.0_dev/repository'.
      'repository'}.
      {'://github.com/dalehenrich/filetree:pharo5.0_dev/repository/x'.
      'repository/x'}.
      {'://github.com/dalehenrich/filetree:pharo5.0_dev\/branch/repository/x'.
      'repository/x'}.
      {'://github.com/dalehenrich/filetree:pharo5.0_dev\/branch'.
      ''}} do: [ :ar | | url expected repoPath |
          url := (scheme , (ar at: 1)) asUrl.
          expected := ar at: 2.
          repoPath := url repositoryPath.
          self assert: repoPath = expected ] ]