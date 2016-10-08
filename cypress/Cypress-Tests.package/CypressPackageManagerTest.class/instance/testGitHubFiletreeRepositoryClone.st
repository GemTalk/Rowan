"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: tests
"
testGitHubFiletreeRepositoryClone
  "self debug: #testGitHubFiletreeRepositoryClone"

  | repo packageNames expected |
  repo := CypressAbstractRepository
    onUrl: 'gitfiletree://github.com/dalehenrich/sample:master/repository' asUrl
    alias: ''.
  packageNames := repo packageNames.
  expected := #('BaseliineOfSample' 'Sample-Core' 'Sample-Tests').
  self assert: packageNames size = expected size.
  expected do: [ :pkgName | packageNames includes: pkgName ]