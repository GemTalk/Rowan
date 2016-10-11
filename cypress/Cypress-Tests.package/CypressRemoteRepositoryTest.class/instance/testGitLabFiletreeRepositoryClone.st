"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: tests
"
testGitLabFiletreeRepositoryClone
  "self debug: #testGitLabFiletreeRepositoryClone"

  | repo packageNames expected |
  repo := CypressAbstractRepository
    onUrl:
      'gitfiletree://gitlab.com/external-sample/external-sample:master/repository'
        asUrl
    alias: ''.
  packageNames := repo packageNames.
  expected := #('BaseliineOfExternal' 'External-Core' 'External-Tests').
  self assert: packageNames size = expected size.
  expected do: [ :pkgName | packageNames includes: pkgName ]