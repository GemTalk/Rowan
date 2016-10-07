"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: tests
"
testGitRepositoryUrlPrintString
  "self debug: #testGitRepositoryUrlPrintString"

  {'gitcypress'.
  'gitfiletree'} do: [ :scheme | {{'://github.com/dalehenrich/filetree'.
      (scheme , '://github.com/dalehenrich/filetree:master/')}.
      {'://github.com/dalehenrich/filetree/repository'.
      (scheme , '://github.com/dalehenrich/filetree:master/repository')}.
      {'://github.com/dalehenrich/filetree:pharo5.0_dev/repository'.
      (scheme , '://github.com/dalehenrich/filetree:pharo5.0_dev/repository')}.
      {'://github.com/dalehenrich/filetree:pharo5.0_dev/repository/x'.
      (scheme , '://github.com/dalehenrich/filetree:pharo5.0_dev/repository/x')}.
      {'://github.com/dalehenrich/filetree:pharo5.0_dev\/branch/repository/x'.
      (scheme
        , '://github.com/dalehenrich/filetree:pharo5.0_dev\/branch/repository/x')}.
      {'://github.com/dalehenrich/filetree:pharo5.0_dev\/branch'.
      (scheme , '://github.com/dalehenrich/filetree:pharo5.0_dev\/branch/')}}
        do: [ :ar | | url expected printString |
          url := (scheme , (ar at: 1)) asUrl.
          expected := ar at: 2.
          printString := url printString.
          self assert: printString = expected ] ]