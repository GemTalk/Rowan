Class {
	#name : 'RwPackageReadSaveTest',
	#superclass : 'RwToolTest',
	#category : 'Rowan-Tode'
}

{ #category : 'private' }
RwPackageReadSaveTest >> doTestReadSaveReadForFormat: format [
  | basicPackageSet readPackageSet myReadSpecification rowanSpec readPackageSet2 diff mySaveSpecification |
  basicPackageSet := self basicPackageSet.
  rowanSpec := RwSpecification fromUrl: 'Rowan'.
  myReadSpecification := self
    setUpSpecificationForReadTest: basicPackageSet
    rowanSpec: rowanSpec
    repositoryFormat: format.
  readPackageSet := RwTool read
    readPackageSetForSpecification: myReadSpecification.
  mySaveSpecification := self
    setUpSpecificationForSaveTest: readPackageSet
    rowanSpec: rowanSpec
    repositoryFormat: format.
  readPackageSet2 := self
    doTestSaveRead: readPackageSet
    mySaveSpecification: mySaveSpecification
    forFormat: format.
  diff := readPackageSet2 compareAgainstBase: readPackageSet.
  self assert: diff isEmpty
]

{ #category : 'private' }
RwPackageReadSaveTest >> doTestSaveRead: savePackageSet mySaveSpecification: mySaveSpecification forFormat: format [
  | readPackageSet diff |
  RwTool write writePackageSet: savePackageSet specification: mySaveSpecification.
  readPackageSet := RwTool read
    readPackageSetForSpecification: mySaveSpecification.
  diff := savePackageSet compareAgainstBase: readPackageSet.
  self assert: diff isEmpty.
  ^ readPackageSet
]

{ #category : 'private' }
RwPackageReadSaveTest >> doTestSaveReadForFormat: format [
  | savePackageSet mySaveSpecification rowanSpec readPackageSet diff |
  savePackageSet := self basicPackageSet.
  rowanSpec := RwSpecification fromUrl: 'Rowan'.
  mySaveSpecification := self
    setUpSpecificationForSaveTest: savePackageSet
    rowanSpec: rowanSpec
    repositoryFormat: format.
  self
    doTestSaveRead: savePackageSet
    mySaveSpecification: mySaveSpecification
    forFormat: format
]

{ #category : 'private' }
RwPackageReadSaveTest >> setUpSpecificationForReadTest: myPackageSet rowanSpec: rowanSpec repositoryFormat: repositoryFormat [
  "read from existing test repositories in testRepositories ... readonly repos"

  ^ self
    setUpSpecificationForTest: myPackageSet
    rowanSpec: rowanSpec
    repositoryFormat: repositoryFormat
    relativeRootPath: '/testRepositories/'
    cleanRoot: false
]

{ #category : 'private' }
RwPackageReadSaveTest >> setUpSpecificationForSaveTest: myPackageSet rowanSpec: rowanSpec repositoryFormat: repositoryFormat [
  "write to empty test directory in testRepositories/empty ... make sure there's the directory is empty"

  ^ self
    setUpSpecificationForTest: myPackageSet
    rowanSpec: rowanSpec
    repositoryFormat: repositoryFormat
    relativeRootPath: '/testRepositories/empty/'
    cleanRoot: true
]

{ #category : 'private' }
RwPackageReadSaveTest >> setUpSpecificationForTest: myPackageSet rowanSpec: rowanSpec repositoryFormat: repositoryFormat relativeRootPath: relativeRootPath cleanRoot: cleanRoot [
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

{ #category : 'tests' }
RwPackageReadSaveTest >> testCypressReadSaveRead [
  "read from committed test repository, save to disk, read from disk and compare package sets along the way"

  self doTestReadSaveReadForFormat: 'cypress'
]

{ #category : 'tests' }
RwPackageReadSaveTest >> testCypressSaveRead [
  "save to disk, read from disk and compare package sets"

  self doTestSaveReadForFormat: 'cypress'
]

{ #category : 'tests' }
RwPackageReadSaveTest >> testFiletreeReadSaveRead [
  "read from committed test repository, save to disk, read from disk and compare package sets along the way"

  self doTestReadSaveReadForFormat: 'filetree'
]

{ #category : 'tests' }
RwPackageReadSaveTest >> testFiletreeSaveRead [
  "save to disk, read from disk and compare package sets"

  self doTestSaveReadForFormat: 'filetree'
]

{ #category : 'tests' }
RwPackageReadSaveTest >> testTonelReadSaveRead [
  "read from committed test repository, save to disk, read from disk and compare package sets along the way"

  self doTestReadSaveReadForFormat: 'tonel'
]

{ #category : 'tests' }
RwPackageReadSaveTest >> testTonelSaveRead [
  "save to disk, read from disk and compare package sets"

  self doTestSaveReadForFormat: 'tonel'
]
