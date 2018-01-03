Class {
	#name : 'RwLibrarian',
	#superclass : 'Object',
	#instVars : [
		'baseLoadSpec',
		'myLoadSpec'
	],
	#category : 'Rowan-Tode'
}

{ #category : 'private' }
RwLibrarian >> _executeCodeLibraryBlock: aBlock [
  "Do not commit during aBlock ... we don't want session methods enabled for SystemUser"

  "for now you need to run as SystemUser"

  | librarian transientSymbolList librarianSessionMethods librarianUserGlobals |
  System
    currentObjectSecurityPolicy: self _userProfile defaultObjectSecurityPolicy.	"assume that only SystemUser can do this"
  librarian := self _librarian.
  librarianUserGlobals := librarian symbolList objectNamed: #'UserGlobals'.
  [ 
  transientSymbolList := GsCurrentSession currentSession symbolList.
  (transientSymbolList objectNamed: #'CodeLibrarianSessionMethods')
    ifNil: [ 
      transientSymbolList createDictionaryNamed: #'CodeLibrarianSessionMethods' at: 0.
      librarianSessionMethods := transientSymbolList
        objectNamed: #'CodeLibrarianSessionMethods'.
      librarianSessionMethods
        at: GsPackagePolicy globalName
          put: (librarianUserGlobals at: GsPackagePolicy globalName);
        yourself.
      GsPackagePolicy current enable ].
  aBlock value ]
    ensure: [ 
      transientSymbolList remove: librarianSessionMethods ifAbsent: [  ].
      GsPackagePolicy current disable ]
]

{ #category : 'private' }
RwLibrarian >> _librarian [
  ^ AllUsers userWithId: 'CodeLibrarianUser' ifAbsent: [ ^ self _myUserProfile ]
]

{ #category : 'private' }
RwLibrarian >> _librarianUserGlobals [
  ^ self librarian symbolList objectNamed: #'UserGlobals'
]

{ #category : 'private' }
RwLibrarian >> _loadMetacello: groupNamesOrNil [
  | librarian metacello |
  librarian := self _librarian.
  metacello := (librarian objectNamed: 'Metacello') new
    baseline: self _projectName;
    repository: self _repositoryUrl;
    yourself.
  ^ groupNamesOrNil
    ifNil: [ metacello load ]
    ifNotNil: [ metacello load: groupNamesOrNil ]
]

{ #category : 'private' }
RwLibrarian >> _loadPackages: packageNames [
  "load the list of packages in the given repository into symbolList of userId"

  | packageManager repo librarian |
  librarian := self _librarian.
  packageManager := (librarian objectNamed: 'CypressEnvironmentPackageManager')
    new.
  repo := (librarian objectNamed: 'CypressAbstractRepository')
    onUrl:
      ((librarian objectNamed: 'CypressUrl') absoluteFromText: self _repositoryUrl)
    alias: ''.
  packageManager
    defaultSymbolDictionaryName: self _symbolDictName asSymbol;
    defaultSymbolList: self _userProfile symbolList.
  packageNames
    do: [ :packageName | 
      packageManager
        addResolvedReference:
          ((librarian objectNamed: 'CypressResolvedReference')
            name: packageName
            repository: repo) ].
  packageManager loadResolvedReferences.
  ^ true
]

{ #category : 'private' }
RwLibrarian >> _loadUsingSpec: aRwLoadSpecification [

	aRwLoadSpecification loadUsingLibrarian: self
]

{ #category : 'private' }
RwLibrarian >> _myLoadSpec [
  ^ myLoadSpec ifNil: [ myLoadSpec := RwPackageLoadSpecification new ]
]

{ #category : 'private' }
RwLibrarian >> _myUserProfile [
  ^ System myUserProfile
]

{ #category : 'private' }
RwLibrarian >> _theLoadSpec [
	"return the spec to be loaded, if no baseLoadSpec specified, then use the spec defined by the loadDirectives"

	^baseLoadSpec 
		ifNil: [ self _myLoadSpec ]
		ifNotNil: [ baseLoadSpec mergeSpec: self _myLoadSpec ].

]

{ #category : 'load directives' }
RwLibrarian >> baseline: aString [
	myLoadSpec := RwProjectLoadSpecification new
		projectName: aString;
		yourself
]

{ #category : 'actions' }
RwLibrarian >> load [
  "load theLoadSpec"

	^ self _loadUsingSpec: self _theLoadSpec
]

{ #category : 'actions' }
RwLibrarian >> load: packageOrGroupNames [
	"load the list of packages in the given repository into symbolList of userId"

	| theLoadSpec |
	theLoadSpec := self _theLoadSpec.
	theLoadSpec packageOrGroupNamesToLoad: packageOrGroupNames.
	^ self _loadUsingSpec: theLoadSpec
]

{ #category : 'private - loading' }
RwLibrarian >> loadPackageSpecification: aRwPackageLoadSpecification [
]

{ #category : 'private - loading' }
RwLibrarian >> loadProjectSpecification: aRwProjectLoadSpecification [
]

{ #category : 'load directives' }
RwLibrarian >> repository: aRepositoryUrl [
  "specify the repository url which encodes the repository type and the path to the repository"

  self _myLoadSpec repositoryUrl: aRepositoryUrl
]

{ #category : 'actions' }
RwLibrarian >> save [
  "save the project named by eth baseline in the given repository on behalf of the given userId"

  | projectSpec librarian loadedPackages |
  projectName
    ifNil: [ 
      self
        error:
          'The name of the project needs to be specified by the `baseline:` method.' ].
  librarian := self _librarian.
  projectSpec := (librarian objectNamed: 'MetacelloProjectRegistration')
    registry baselineProjectSpecs
    detect: [ :spec | spec name = projectName ]
    ifNone: [ self error: 'registration for projectSpec: ' , projectName , ' not found' ].
  (librarian objectNamed: 'MetacelloProjectRegistration')
    registrationForProjectSpec: projectSpec
    ifAbsent: [ self error: 'registration for projectSpec: ' , projectSpec name , ' not found' ]
    ifPresent: [ :registration :ignored | 
      loadedPackages := registration projectSpec version packages
        select: [ :pkg | pkg isPackageLoaded ].
      loadedPackages addFirst: registration projectSpec projectPackage ].
  loadedPackages isEmpty
    ifTrue: [ self error: 'No loaded packages to save' ].
  self save: (loadedPackages collect: [ :each | each file ])
]

{ #category : 'actions' }
RwLibrarian >> save: packageNames [
  "save the list of packages in the given repository on behalf of the given userId"

  self
    _executeCodeLibraryBlock: [ 
      | librarian repo |
      librarian := self _librarian.
      repo := (librarian objectNamed: 'CypressAbstractRepository')
        onUrl: ((librarian objectNamed: 'CypressUrl') for: self _repositoryUrl)
        alias: 'fnord'.
      packageNames
        do: [ :packageName | 
          | package |
          package := (librarian
            objectNamed: 'CypressEnvironmentPackageDefinition')
            ifNotNil: [ :cl | 
              (cl named: packageName)
                lookupSymbolList: self _userProfile symbolList;
                yourself ]
            ifNil: [ (librarian objectNamed: 'CypressPackageDefinition') named: packageName ].
          repo
            writePackageStructure:
              ((librarian objectNamed: 'CypressPackageStructure') fromPackage: package) ] ]
]

{ #category : 'load directives' }
RwLibrarian >> spec: aSpecNameOrUrl [
  baseLoadSpec := RwLoadSpecification fromUrl: aSpecNameOrUrl
]

{ #category : 'load directives' }
RwLibrarian >> symbolDictName: aString [
  self _mySpec symbolDictName: aString
]

{ #category : 'load directives' }
RwLibrarian >> user: aUserId [
  "specify the userId of the user where the package is installed"

  self _myLoadSpec userId: aUserId
]
