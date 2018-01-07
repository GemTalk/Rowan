Class {
	#name : 'RwPackageReadWriteTest',
	#superclass : 'RwToolTest',
	#category : 'Rowan-Tode'
}

{ #category : 'private' }
RwPackageReadWriteTest >> doTestReadWriteReadForFormat: format [
  | basicPackageSet readPackageSet myReadSpecification rowanSpec readPackageSet2 diff myWriteSpecification |
  basicPackageSet := self basicPackageSet.
  rowanSpec := RwSpecification fromUrl: 'Rowan'.
  myReadSpecification := self
    setUpSpecificationForReadTest: basicPackageSet
    rowanSpec: rowanSpec
    repositoryFormat: format.
  readPackageSet := Rowan tools read
    readPackageSetForSpecification: myReadSpecification.
  myWriteSpecification := self
    setUpSpecificationForWriteTest: readPackageSet
    rowanSpec: rowanSpec
    repositoryFormat: format.
  readPackageSet2 := self
    doTestWriteRead: readPackageSet
    myWriteSpecification: myWriteSpecification
    forFormat: format.
  diff := readPackageSet2 compareAgainstBase: readPackageSet.
  self assert: diff isEmpty
]

{ #category : 'private' }
RwPackageReadWriteTest >> doTestWriteRead: writePackageSet myWriteSpecification: myWriteSpecification forFormat: format [
  | readPackageSet diff |
  Rowan tools write
    writePackageSet: writePackageSet
    specification: myWriteSpecification.
  readPackageSet := Rowan tools read
    readPackageSetForSpecification: myWriteSpecification.
  diff := writePackageSet compareAgainstBase: readPackageSet.
  self assert: diff isEmpty.
  ^ readPackageSet
]

{ #category : 'private' }
RwPackageReadWriteTest >> doTestWriteReadForFormat: format [
  | writePackageSet myWriteSpecification rowanSpec readPackageSet diff |
  writePackageSet := self basicPackageSet.
  rowanSpec := RwSpecification fromUrl: 'Rowan'.
  myWriteSpecification := self
    setUpSpecificationForWriteTest: writePackageSet
    rowanSpec: rowanSpec
    repositoryFormat: format.
  self
    doTestWriteRead: writePackageSet
    myWriteSpecification: myWriteSpecification
    forFormat: format
]

{ #category : 'private' }
RwPackageReadWriteTest >> setUpSpecificationForReadTest: myPackageSet rowanSpec: rowanSpec repositoryFormat: repositoryFormat [
  "read from existing test repositories in testRepositories ... readonly repos"

  ^ self
    setUpSpecificationForTest: myPackageSet
    rowanSpec: rowanSpec
    repositoryFormat: repositoryFormat
    relativeRootPath: '/testRepositories/'
    cleanRoot: false
]

{ #category : 'private' }
RwPackageReadWriteTest >> setUpSpecificationForTest: myPackageSet rowanSpec: rowanSpec repositoryFormat: repositoryFormat relativeRootPath: relativeRootPath cleanRoot: cleanRoot [
  | packageNames repositoryRootPath repositoryPath |
  packageNames := myPackageSet packages keys asArray.
  repositoryRootPath := rowanSpec repositoryRootPath , relativeRootPath.
  repositoryPath := repositoryRootPath , '/' , repositoryFormat , '/'.
  cleanRoot
    ifTrue: [ 
      (CypressFileUtilities current directoryExists: repositoryPath)
        ifTrue: [ CypressFileUtilities current deleteAll: repositoryPath ] ].
  ^ (self basicPackageLoadSpec: packageNames packageNamesToLoad: packageNames)
    repositoryRootPath: repositoryRootPath;
    repositoryUrl: repositoryFormat , ':' , repositoryPath;
    repoPath: 'repositoryFormat';
    yourself
]

{ #category : 'private' }
RwPackageReadWriteTest >> setUpSpecificationForWriteTest: myPackageSet rowanSpec: rowanSpec repositoryFormat: repositoryFormat [
  "write to empty test directory in testRepositories/empty ... make sure there's the directory is empty"

  ^ self
    setUpSpecificationForTest: myPackageSet
    rowanSpec: rowanSpec
    repositoryFormat: repositoryFormat
    relativeRootPath: '/testRepositories/empty/'
    cleanRoot: true
]

{ #category : 'tests' }
RwPackageReadWriteTest >> testCypressReadWriteRead [
  "read from committed test repository, write to disk, read from disk and compare package sets along the way"

  self doTestReadWriteReadForFormat: 'cypress'
]

{ #category : 'tests' }
RwPackageReadWriteTest >> testCypressWriteRead [
  "write to disk, read from disk and compare package sets"

  self doTestWriteReadForFormat: 'cypress'
]

{ #category : 'tests' }
RwPackageReadWriteTest >> testFiletreeReadWriteRead [
  "read from committed test repository, write to disk, read from disk and compare package sets along the way"

  self doTestReadWriteReadForFormat: 'filetree'
]

{ #category : 'tests' }
RwPackageReadWriteTest >> testFiletreeWriteRead [
  "write to disk, read from disk and compare package sets"

  self doTestWriteReadForFormat: 'filetree'
]

{ #category : 'tests' }
RwPackageReadWriteTest >> testSpecReadValidation [
  | spec tool |
  tool := Rowan projectTools read.
  spec := RwPackageLoadSpecification new
    specName: 'Empty';
    register.
  self should: [ tool validate: spec ] raise: Error.
  spec specName: 'test spec for ' , self class name.
  self should: [ tool validate: spec ] raise: Error.
  spec repositoryRootPath: '/tmp/repo'.
  tool validate: spec
]

{ #category : 'tests' }
RwPackageReadWriteTest >> testSpecWriteValidation [
  | spec tool |
  tool := Rowan projectTools write.
  spec := RwPackageLoadSpecification new
    specName: 'Empty';
    register.
  self should: [ tool validate: spec ] raise: Error.
  spec specName: 'test spec for ' , self class name.
  self should: [ tool validate: spec ] raise: Error.
  spec repositoryRootPath: '/tmp/repo'.
  tool validate: spec
]

{ #category : 'tests' }
RwPackageReadWriteTest >> testTonelReadWriteRead [
  "read from committed test repository, write to disk, read from disk and compare package sets along the way"

  self doTestReadWriteReadForFormat: 'tonel'
]

{ #category : 'tests' }
RwPackageReadWriteTest >> testTonelWriteRead [
  "write to disk, read from disk and compare package sets"

  self doTestWriteReadForFormat: 'tonel'
]
