"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: tests
"
testBitBucketFiletreeRepositoryClone
  "self debug: #testBitBucketFiletreeRepositoryClone"

  | repo packageNames expected |
  repo := CypressAbstractRepository
    onUrl:
      'gitfiletree://bitbucket.org/dalehenrich/external:master/repository' asUrl
    alias: ''.
  packageNames := repo packageNames.
  expected := #('BaseliineOfExternal' 'External-Core' 'External-Tests').
  self assert: packageNames size = expected size.
  expected do: [ :pkgName | packageNames includes: pkgName ]