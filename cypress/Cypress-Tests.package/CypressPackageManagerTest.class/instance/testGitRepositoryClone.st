"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: tests
"
testGitRepositoryClone
  "self debug: #testGitRepositoryClone"

  | repo |
  repo := CypressAbstractRepository
    onUrl: 'gitfiletree://github.com/dalehenrich/sample:master/repository' asUrl
    alias: ''