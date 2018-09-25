! Package: CypressTonel-Core


! Remove existing behavior from package CypressTonel-Core
!!!! This can be cleaned up when some package functionality is moved to the base system.

doit
| packageName |
packageName := 'CypressTonel-Core'.
System myUserProfile symbolList do: [:symDict |
	symDict do: [:possibleClass |
			| toRemove |
		possibleClass isBehavior ifTrue: [
			{possibleClass. possibleClass class} do: [:aClass |
				aClass category = packageName
					ifTrue: [
							"*anythingbutpackagename[-anything]"
						toRemove := aClass categoryNames select: 
										[:each |
										each isEmpty not and: [
											(each first = $* and: [(each size = (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2])
														or: [each size > (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2 and: [(each at: packageName size + 2) = $-]]]])
											or: [each first ~= $*]]]
					]
					ifFalse: [
							"*packagename[-anything]"
						toRemove := aClass categoryNames select: 
										[:each |
										each isEmpty not and: [
											each first = $* and: [(each size = (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2])
														or: [each size > (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2 and: [(each at: packageName size + 2) = $-]]]]]]
					].
				toRemove do: [:each | aClass removeCategory: each].
			]
		]
	]
].
true.
%


! Class Declarations

doit
(CypressAbstractFileUrl
	subclass: 'CypressTonelFileUrl'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'CypressTonel-Core';
		comment: 'All Cypress classes are private to GemStone and are likely to be removed in a future release.';
		immediateInvariant.
true.
%

doit
(CypressAbstractPackageFiler
	subclass: 'TonelCypressReader'
	instVarNames: #( packageName definitions directoryPath )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'CypressTonel-Core';
		comment: 'I''m a reader for tonel format.';
		immediateInvariant.
true.
%

doit
(CypressAbstractPackageWriter
	subclass: 'TonelCypressWriter'
	instVarNames: #( snapshot sourceDir packageDir writer )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'CypressTonel-Core';
		comment: 'I''m a writer for tonel format';
		immediateInvariant.
true.
%

doit
(CypressFileSystemRepository
	subclass: 'CypressTonelRepository'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #())
		category: 'CypressTonel-Core';
		comment: 'No class-specific documentation for CypressTonelRepository, hierarchy is: 
Object
  CypressAbstractRepository( url properties readerClass writerClass)
    CypressFileSystemRepository( directoryPath)
      CypressTonelRepository
';
		immediateInvariant.
true.
%

! Class Implementation for CypressTonelFileUrl

! ------------------- Class methods for CypressTonelFileUrl

category: 'constants'
classmethod: CypressTonelFileUrl
schemeName
  ^ 'tonel'
%

! ------------------- Instance methods for CypressTonelFileUrl

category: 'accessing'
method: CypressTonelFileUrl
codeFormat
  ^ 'Tonel'
%

category: 'testing'
method: CypressTonelFileUrl
isStrict
  ^ true
%

category: 'accessing'
method: CypressTonelFileUrl
repositoryClass

	^CypressTonelRepository
%

! Class Implementation for TonelCypressReader

! ------------------- Class methods for TonelCypressReader

category: 'definition creation'
classmethod: TonelCypressReader
definitionForType: aString
  aString = TonelWriter classLabel
    ifTrue: [ ^ CypressClassDefinition ].
  aString = TonelWriter extensionLabel
    ifTrue: [ ^ nil ].
  TonelParseError signal: 'Unknown type declaration.'
%

category: 'accessing'
classmethod: TonelCypressReader
definitionOrders
	"Used to sort definitions inside a snapshot"
	^ Dictionary new
		at: CypressMethodDefinition put: 1;
		at: CypressClassDefinition put: 2;
		yourself
%

category: 'definition creation'
classmethod: TonelCypressReader
newClassDefinitionForClassNamed: nameString superclassName: superclassString category: categoryString instVarNames: ivarArray classVarNames: cvarArray poolDictionaryNames: poolArray classInstVarNames: civarArray type: typeSymbol comment: commentString
  ^ CypressClassDefinition
    name: nameString
    superclassName: superclassString
    category: categoryString
    instVarNames: ivarArray
    classInstVarNames: civarArray
    classVarNames: cvarArray
    poolDictionaryNames: poolArray
    comment: commentString
    type: typeSymbol
%

category: 'definition creation'
classmethod: TonelCypressReader
newClassDefinitionFrom: anArray
  | metadata |
  metadata := anArray sixth.
  ^ CypressClassDefinition
    name: (metadata at: #'name')
    superclassName: (metadata at: #'superclass')
    category: (metadata at: #'category')
    instVarNames: (metadata at: #'instvars' ifAbsent: [ #() ])
    classInstVarNames: (metadata at: #'classinstvars' ifAbsent: [ #() ])
    classVarNames: (metadata at: #'classvars' ifAbsent: [ #() ])
    poolDictionaryNames: (metadata at: #'pools' ifAbsent: [ #() ])
    gs_options: (metadata at: #'gs_options' ifAbsent: [ #() ])
    gs_constraints: (metadata at: #'gs_constraints' ifAbsent: [ #() ])
    comment: (anArray second ifNil: [ '' ])
    type: (metadata at: #'type' ifAbsent: [ #'normal' ]) asSymbol
%

category: 'definition creation'
classmethod: TonelCypressReader
newMethodDefinitionForClassNamed: className classIsMeta: meta selector: selector category: category source: source
  ^ CypressMethodDefinition
    className: className
    classIsMeta: meta
    selector: selector
    category: category
    source: source
%

category: 'definition creation'
classmethod: TonelCypressReader
newTypeDefinitionFrom: anArray
  | typeClass |
  typeClass := self definitionForType: anArray fourth.
  typeClass = CypressClassDefinition
    ifTrue: [ ^ self newClassDefinitionFrom: anArray ].
  "is extension, no type"
  ^ nil
%

category: 'reading'
classmethod: TonelCypressReader
on: rootDirectoryPath fileName: packageName
  | reader |
  reader := self new.
  ^ reader
    packageDirectory:
        rootDirectoryPath , reader fileUtils pathNameDelimiter , packageName;
    packageName: packageName;
    yourself
%

! ------------------- Instance methods for TonelCypressReader

category: 'private'
method: TonelCypressReader
canBeLoaded: aFileReference
  | fileName |
  fileName := self fileUtils localNameFrom: aFileReference.
  ^ fileName ~= 'package.st' and: [ fileName endsWith: '.st' ]
%

category: 'private'
method: TonelCypressReader
categoriesFrom: aCollection
  ^ ((aCollection select: [ :each | each isClassDefinition ])
    collect: [ :each | each category asSymbol ]) asSet sortWithBlock: [ :a :b | a < b ]
%

category: 'parser support'
method: TonelCypressReader
definitionForType: aString
  ^ self class definitionForType: aString
%

category: 'accessing'
method: TonelCypressReader
definitions
  ^ definitions
%

category: 'accessing'
method: TonelCypressReader
directoryPath
  ^ directoryPath
%

category: 'accessing'
method: TonelCypressReader
directoryPath: aString
  directoryPath := aString
%

category: 'initializing - private'
method: TonelCypressReader
initializeForRepository: aCypressFileSystemRepository
  repository := aCypressFileSystemRepository.
  self directoryPath: repository directoryPath
%

category: 'loading'
method: TonelCypressReader
loadDefinitions
  "load all definitions in the known directories and files."
  | definitionOrders newDefinitions loadable dir |
  definitionOrders := self class definitionOrders.
  loadable := (self fileUtils directoryEntriesFrom: (dir := self packageDirectory))
               select: [ :each | self canBeLoaded: each ].
  loadable isEmpty ifTrue:[ GsFile gciLogServer:'No loadable files found in ' , dir asString ].
  newDefinitions := ( 
    loadable collect: [ :each | 
    GsFile gciLogServer:'Reading ' , each asString .
      self fileUtils
        readStreamFor: each
        do: [ :s | TonelParser parseStream: s forReader: self ] ])
    flattened
    sort: [ :a :b | (definitionOrders at: a class) < (definitionOrders at: b class) ].
  definitions := newDefinitions sort
%

category: 'loading'
method: TonelCypressReader
loadDefinitionsFromFiles: fileNames
  "From the known directories and files, load only those files
   whose file names end with one of the Strings in the Array  fileNames ."
  | definitionOrders newDefinitions loadable dir |
  definitionOrders := self class definitionOrders.
  loadable := (self fileUtils directoryEntriesFrom: (dir := self packageDirectory))
        select: [ :each | (self canBeLoaded: each) and:[ 
                  (fileNames detect:[:n | each endsWith: n] ifNone:[nil]) ~~ nil] ].
  loadable isEmpty ifTrue:[ 
      GsFile gciLogServer: 'None of specified  files found in ' , dir asString ].
  newDefinitions := ( 
    loadable collect: [ :each | 
    GsFile gciLogServer:'Reading ' , each asString .
      self fileUtils
        readStreamFor: each
        do: [ :s | TonelParser parseStream: s forReader: self ] ])
    flattened
    sort: [ :a :b | (definitionOrders at: a class) < (definitionOrders at: b class) ].
  definitions := newDefinitions sort
%


category: 'parser support'
method: TonelCypressReader
newClassDefinitionFrom: anArray
	^self class newClassDefinitionFrom: anArray
%

category: 'parser support'
method: TonelCypressReader
newMethodDefinitionForClassNamed: className classIsMeta: meta selector: selector category: category source: source
  ^ self class
    newMethodDefinitionForClassNamed: className
    classIsMeta: meta
    selector: selector
    category: category
    source: source
%

category: 'parser support'
method: TonelCypressReader
newTraitDefinitionFrom: anArray
	^ self class newTraitDefinitionFrom: anArray
%

category: 'parser support'
method: TonelCypressReader
newTypeDefinitionFrom: anArray
	^ self class newTypeDefinitionFrom: anArray
%

category: 'accessing'
method: TonelCypressReader
packageName

   ^packageName
%

category: 'accessing'
method: TonelCypressReader
packageName: aString
  packageName := aString.
  self
    packageDirectory:
      (self fileUtils directoryFromPath: packageName relativeTo: self directoryPath)
%

category: 'reading'
method: TonelCypressReader
readPackageStructure
  self loadDefinitions.
  ^ packageStructure
    fromSnapshot: definitions;
    yourself
%

! Class Implementation for TonelCypressWriter

! ------------------- Class methods for TonelCypressWriter

category: 'instance creation'
classmethod: TonelCypressWriter
on: sourceDir
	^ self new
		sourceDir: sourceDir;
		yourself
%

! ------------------- Instance methods for TonelCypressWriter

category: 'accessing'
method: TonelCypressWriter
definitions

	^ snapshot definitions
%

category: 'initializing - private'
method: TonelCypressWriter
initializeForRepository: aCypressFileSystemRepository
  repository := aCypressFileSystemRepository.
  self sourceDir: repository directoryPath
%

category: 'private testing'
method: TonelCypressWriter
isClass: aClassDefinition
	^ aClassDefinition class = CypressClassDefinition
%

category: 'private testing'
method: TonelCypressWriter
isTrait: aClassDefinition
	^ false
%

category: 'accessing'
method: TonelCypressWriter
packageDir
	^ packageDir 
		ifNotNil: [ self fileUtils directoryFromPath: packageDir relativeTo: self sourceDir ]
		ifNil: [ self sourceDir  ]
%

category: 'accessing'
method: TonelCypressWriter
sourceDir
	^ sourceDir
%

category: 'accessing'
method: TonelCypressWriter
sourceDir: aFileReference
	sourceDir := aFileReference
%

category: 'private writing'
method: TonelCypressWriter
writeMethodExtensions
	| classesAndMethods |
	classesAndMethods := Dictionary new.
	snapshot definitions 
		select: [ :each | each isMethodDefinition and: [ each isExtensionMethod ] ]
		thenDo: [ :each | 
			(classesAndMethods 
				at: each className
				ifAbsentPut: [ Set new ])
				add: each ].
	classesAndMethods keysAndValuesDo: [ :className :methods | 
		self writer
			writeExtensionMethods: methods 
			className: className ]
		
	
%

category: 'private writing'
method: TonelCypressWriter
writePackage: aPackageName
	"I'm assuming first category is actually the package"
	packageDir := aPackageName.
	(self fileUtils directoryExists: self packageDir) ifTrue: [ 
		self fileUtils deleteAll: self packageDir ].  
	self fileUtils ensureDirectoryExists: self packageDir.
	self writer writePackage: packageDir
%

category: 'private writing'
method: TonelCypressWriter
writePackageFileNamed: packageFileName do: writerBlock

	self fileUtils 
		writeStreamFor: packageFileName
		in: self packageDir 
		do: [ :aStream | writerBlock value: aStream ]
%

category: 'accessing'
method: TonelCypressWriter
writer
  ^ writer
    ifNil: [ 
      writer := TonelWriter new
        packageWriter: self;
        yourself ]
%

category: 'accessing'
method: TonelCypressWriter
writer: aTonelWriter
	writer := aTonelWriter
%

category: 'writing'
method: TonelCypressWriter
writeSnapshot: aSnapshot
  self
    writeSnapshot: aSnapshot
    inPackageNamed: (aSnapshot dynamicInstVarAt: #'packageName')
%

category: 'writing'
method: TonelCypressWriter
writeSnapshot: aSnapshot inPackageNamed: packageName
  snapshot := aSnapshot.	"ensure package dirs exists. 
	 It has to be just one but well..."
  self writePackage: packageName.	"now export classes"
  (self definitions select: [ :each | each isClassDefinition ])
    do: [ :each | self writer writeClass: each ].	"... and method extensions"
  self writeMethodExtensions
%

category: 'writing'
method: TonelCypressWriter
writeVersion: aVersion
	self writeSnapshot: aVersion snapshot
%

! Class Implementation for CypressTonelRepository

! ------------------- Instance methods for CypressTonelRepository

category: 'updating properties'
method: CypressTonelRepository
codeFormatProperty: aString
  (aString equalsNoCase: 'Tonel')
    ifFalse: [ ^ super codeFormatProperty: aString ].
  properties at: '_gs_format' put: aString
%

category: 'initializing - private'
method: CypressTonelRepository
initializeReaderAndWriterClasses
  self isCodeFormatTonel
    ifTrue: [ 
      readerClass := TonelCypressReader.
      writerClass := TonelCypressWriter ]
    ifFalse: [ super initializeReaderAndWriterClasses ]
%

category: 'testing properties'
method: CypressTonelRepository
isCodeFormatTonel

	^self isCodeFormatProperty: 'Tonel'
%

category: 'accessing properties'
method: CypressTonelRepository
packageExtension
  ^ properties at: 'packageExtension' ifAbsent: [ '' ]
%

category: 'reading'
method: CypressTonelRepository
readPackageStructureForPackageNamed: packageName 
  | reader |
  reader := self reader
    packageName: packageName;
    loadDefinitions;
    yourself.
  ^ CypressSnapshot definitions: reader definitions
%
method: CypressTonelRepository
readPackageStructureForPackageNamed: packageName  files: fileNames
  "Load only those files whose file names end with one of the Strings
   in the Array fileNames."
  | reader |
  reader := self reader
    packageName: packageName;
    loadDefinitionsFromFiles: fileNames  ;
    yourself.
  ^ CypressSnapshot definitions: reader definitions
%

category: 'reading'
method: CypressTonelRepository
readPropertiesFile
  "noop"

%

category: 'reading'
method: CypressTonelRepository
readPropertiesFile: fileName
  "noop"

%

category: 'writing'
method: CypressTonelRepository
writePackageStructure: aPackageStructure
  ^ self writer
    writeSnapshot: aPackageStructure snapshot
    inPackageNamed: aPackageStructure packageName
%

category: 'writing'
method: CypressTonelRepository
writePropertiesFile: fileName
  "noop"

%

! Class Extensions

! Class Extension for CypressClassDefinition

! ------------------- Class methods for CypressClassDefinition

category: '*cypresstonel-core'
classmethod: CypressClassDefinition
name: aClassName superclassName: aSuperclassName category: aCategory instVarNames: someInstanceVariableNames classInstVarNames: someClassInstanceVariableNames classVarNames: someClassVariableNames poolDictionaryNames: somePoolDictionaryNames comment: aComment type: type
  | st |
  st := type.
  st == #'normal'
    ifTrue: [ st := '' ].
  ^ self
    name: aClassName
    superclassName: aSuperclassName
    category: aCategory
    instVarNames: someInstanceVariableNames
    classInstVarNames: someClassInstanceVariableNames
    classVarNames: someClassVariableNames
    poolDictionaryNames: somePoolDictionaryNames
    comment: aComment
    subclassType: st
%

category: '*cypresstonel-core'
classmethod: CypressClassDefinition
name: aClassName superclassName: aSuperclassName category: aCategory instVarNames: someInstanceVariableNames 
	classInstVarNames: someClassInstanceVariableNames classVarNames: someClassVariableNames 
	poolDictionaryNames: somePoolDictionaryNames  gs_options: someGs_options gs_constraints: someGs_constraints
	comment: aComment type: type
  | st |
  st := type.
  st == #'normal'
    ifTrue: [ st := '' ].
  ^ self
    name: aClassName
    superclassName: aSuperclassName
    category: aCategory
    instVarNames: someInstanceVariableNames
    classInstVarNames: someClassInstanceVariableNames
    classVarNames: someClassVariableNames
    poolDictionaryNames: somePoolDictionaryNames
    gs_options: someGs_options 
    gs_constraints: someGs_constraints
    comment: aComment
    subclassType: st
%

category: '*cypresstonel-core'
classmethod: CypressClassDefinition
name: aClassName superclassName: aSuperclassName category: aCategory instVarNames: someInstanceVariableNames classInstVarNames: someClassInstanceVariableNames classVarNames: someClassVariableNames poolDictionaryNames: somePoolDictionaryNames comment: aComment subclassType: subclassType

	^ self new
		name: aClassName asString
		superclassName: aSuperclassName asString
		category: aCategory asString
		instVarNames:
			(someInstanceVariableNames asArray collect: [ :each | each asString ])
		classInstVarNames:
			(someClassInstanceVariableNames asArray collect: [ :each | each asString ])
		classVarNames:
			(someClassVariableNames asArray collect: [ :each | each asString ])
		poolDictionaryNames:
			(somePoolDictionaryNames asArray collect: [ :each | each asString ])
		comment: (self normalizeLineEndingsOf: aComment)
		subclassType: subclassType asString
%

category: '*cypresstonel-core'
classmethod: CypressClassDefinition
name: aClassName superclassName: aSuperclassName category: aCategory instVarNames: someInstanceVariableNames 
	classInstVarNames: someClassInstanceVariableNames classVarNames: someClassVariableNames 
	poolDictionaryNames: somePoolDictionaryNames gs_options: gs_options gs_constraints: gs_constraints comment: aComment 
	subclassType: subclassType

	^ self new
		name: aClassName asString
		superclassName: aSuperclassName asString
		category: aCategory asString
		instVarNames:
			(someInstanceVariableNames asArray collect: [ :each | each asString ])
		classInstVarNames:
			(someClassInstanceVariableNames asArray collect: [ :each | each asString ])
		classVarNames:
			(someClassVariableNames asArray collect: [ :each | each asString ])
		poolDictionaryNames:
			(somePoolDictionaryNames asArray collect: [ :each | each asString ])
		gs_options: gs_options 
		gs_constraints: gs_constraints
		comment: (self normalizeLineEndingsOf: aComment)
		subclassType: subclassType asString
%

! ------------------- Instance methods for CypressClassDefinition

category: '*cypresstonel-core'
method: CypressClassDefinition
hasClassTraitComposition

	^false
%

category: '*cypresstonel-core'
method: CypressClassDefinition
hasComment
  ^ comment notNil and: [ comment ~= '' ]
%

category: '*cypresstonel-core'
method: CypressClassDefinition
hasTraitComposition

	^false
%

category: '*cypresstonel-core'
method: CypressClassDefinition
isClassDefinition
  ^ true
%

category: '*cypresstonel-core'
method: CypressClassDefinition
poolDictionaries

	^self poolDictionaryNames
%

category: '*cypresstonel-core'
method: CypressClassDefinition
sortKey
	^ self className
%

category: '*cypresstonel-core'
method: CypressClassDefinition
type
  | st |
  st := self subclassType.
  st = ''
    ifTrue: [ ^ #'normal' ].
  ^ st
%

! Class Extension for CypressDefinition

! ------------------- Instance methods for CypressDefinition

category: '*cypresstonel-core'
method: CypressDefinition
<= other
	^ self sortKey <= other sortKey
%

category: '*cypresstonel-core'
method: CypressDefinition
isClassDefinition
  ^ false
%

category: '*cypresstonel-core'
method: CypressDefinition
isMethodDefinition
  ^ false
%

category: '*cypresstonel-core'
method: CypressDefinition
isTraitDefinition
  ^ false
%

category: '*cypresstonel-core'
method: CypressDefinition
sortKey
	self subclassResponsibility
%

! Class Extension for CypressGemStoneDirectoryUtilities

! ------------------- Class methods for CypressGemStoneDirectoryUtilities

category: '*cypresstonel-core'
classmethod: CypressGemStoneDirectoryUtilities
default
  ^ self workingDirectory
%

category: '*cypresstonel-core'
classmethod: CypressGemStoneDirectoryUtilities
entryNamesFrom: aDirectory
  "Answer just the name of the contents of aDirectory."

  ^ (((self directoryEntriesFrom: aDirectory)
    collect: [ :each | self localNameFrom: each ])
    reject: [ :each | each = '.' or: [ each = '..' ] ])
    sortWithBlock: [ :a :b | a <= b ]
%

! Class Extension for CypressMethodDefinition

! ------------------- Instance methods for CypressMethodDefinition

category: '*cypresstonel-core'
method: CypressMethodDefinition
fullClassName
	
	^ self classIsMeta
		ifFalse: [self className]
		ifTrue: [self className, ' class' ]
%

category: '*cypresstonel-core'
method: CypressMethodDefinition
isExtensionMethod
	^ category beginsWith: '*'
%

category: '*cypresstonel-core'
method: CypressMethodDefinition
isMethodDefinition
  ^ true
%

category: '*cypresstonel-core'
method: CypressMethodDefinition
sortKey
	^ self className, '.', (self classIsMeta ifTrue: ['meta'] ifFalse: ['nonmeta']), '.', self selector
%

! Class Extension for CypressSnapshot

! ------------------- Instance methods for CypressSnapshot

category: '*cypresstonel-core'
method: CypressSnapshot
snapshot
  ^ self
%

! Class initializers 

doit
true.
%



! End of Package: CypressTonel-Core


