"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: tests
"
testGitRepositoryUrlHost
  "self debug: #testGitRepositoryUrlHost"

  {'gitcypress'.
  'gitfiletree'} do: [ :scheme | {{'://github.com/dalehenrich/filetree'.
      'github.com'}.
      {'://bitbucket.com/dalehenrich/filetree/repository'.
      'bitbucket.com'}} do: [ :ar | | url expected host |
          url := (scheme , (ar at: 1)) asUrl.
          expected := ar at: 2.
          host := url host.
          self assert: host = expected ] ]