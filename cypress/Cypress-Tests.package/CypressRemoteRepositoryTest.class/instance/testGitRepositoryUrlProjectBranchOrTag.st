"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: tests
"
testGitRepositoryUrlProjectBranchOrTag
  "self debug: #testGitRepositoryUrlProjectBranchOrTag"

  {'gitcypress'.
  'gitfiletree'} do: [ :scheme | {{'://github.com/dalehenrich/filetree'.
      'master'}.
      {'://github.com/dalehenrich/filetree/repository'.
      'master'}.
      {'://github.com/dalehenrich/filetree:pharo5.0_dev/repository'.
      'pharo5.0_dev'}.
      {'://github.com/dalehenrich/filetree:pharo5.0_dev/repository/x'.
      'pharo5.0_dev'}.
      {'://github.com/dalehenrich/filetree:pharo5.0_dev\/branch/repository/x'.
      'pharo5.0_dev/branch'}.
      {'://github.com/dalehenrich/filetree:pharo5.0_dev\/branch'.
      'pharo5.0_dev/branch'}} do: [ :ar | | url expected projectBranchOrTag |
          url := (scheme , (ar at: 1)) asUrl.
          expected := ar at: 2.
          projectBranchOrTag := url projectBranchOrTag.
          self assert: projectBranchOrTag = expected ] ]