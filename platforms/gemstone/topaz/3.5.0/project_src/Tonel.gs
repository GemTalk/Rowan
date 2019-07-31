! Class Declarations
! Generated file, do not Edit

doit
(CypressAbstractFileUrl
	subclass: 'CypressTonelFileUrl'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
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
	options: #()
)
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
	options: #()
)
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
	options: #()
)
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

doit
(Error
	subclass: 'TonelParseError'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Tonel-Core';
		comment: 'I''m a parsing error. 
I happen whenever the parsing of a tonel file is broken in someway.';
		immediateInvariant.
true.
%

doit
(Notification
	subclass: 'TonelShouldIgnore'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Tonel-Core';
		comment: 'I''m a notification to say tonel writer that he should ignore a section. 
This tipically happens on a MCClassTraitDefinition, because it will be managed on MCTraitDefinition.

(see TonelWriter>>typeOf:)';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'GsTonelOrderedDictionary'
	instVarNames: #( size keys values )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Tonel-GemStoneCommon-Core';
		comment: 'I am an implementation of a dictionary. Compared to other dictionaries I am very efficient for small sizes, speed- and space-wise. I also mantain the order in which elements are added when iterating. My implementation features some ideas from the RefactoringBrowser.';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'TonelParser'
	instVarNames: #( packageReader stream lastSelectorParsed )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Tonel-Core';
		comment: 'I''m a parser for tonel files. 
I parse a class with the following format: 

Tonel spec
====

    [comment]
    type { typeDefinition }
    (
        [{ methodMetadata }]
        method [
            methodBody ] 
    )*


comment
---
"
comment string
"
is optional (but it should be there, in good design ;)

type
---
Class|Trait|Extension

typeDefinition
---
a STON file with class/trait/extension metadata

methodMetadata
---
a STON file with method metadata
is optional (but also, recommended)

method
---
method declaration as this: 

Class[ class] >> selector

methodBody 
---
the method body (we do not parse contents, that''s class builder task)';
		immediateInvariant.
true.
%

doit
(Object
	subclass: 'TonelWriter'
	instVarNames: #( packageWriter )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Tonel-Core';
		comment: '';
		immediateInvariant.
true.
%

doit
(STONWriter
	subclass: 'TonelSTONWriter'
	instVarNames: #( aliases )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Tonel-Core';
		comment: 'I''m a modified STON writer to make tonel metadata look as we want.

- it accept aliasses for classes, so I can say OrderedDictionary -> nil (then I do not have an extra information I do not want). Btw, tonel needs to use ordered dictionaries instead plain dictionaries because output needs to be deterministic, and we want to control the order of attributes we publish.
- if dictionary has just one element, it prints it in just one line, to have a more compact view.';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'TonelAbstractTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Tonel-Tests';
		comment: '';
		immediateInvariant.
true.
%

doit
(TonelAbstractTest
	subclass: 'TonelAbstractWriterTest'
	instVarNames: #( directory )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Tonel-Tests';
		comment: '';
		immediateInvariant.
true.
%

doit
(TonelAbstractWriterTest
	subclass: 'TonelCypressWriterTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'CypressTonel-Tests';
		comment: '';
		immediateInvariant.
true.
%

doit
(TonelAbstractTest
	subclass: 'TonelReaderTest'
	instVarNames: #( directory )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Tonel-Tests';
		comment: '';
		immediateInvariant.
true.
%

doit
(TonelReaderTest
	subclass: 'TonelCypressReaderTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'CypressTonel-Tests';
		comment: '';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'TonelParserTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Tonel-Tests';
		comment: '';
		immediateInvariant.
true.
%

doit
(TonelParserTest
	subclass: 'TonelParserForCypressTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Tonel-Cypress-Tests';
		comment: '';
		immediateInvariant.
true.
%

doit
(TestCase
	subclass: 'TonelWriterTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Tonel-Tests';
		comment: '';
		immediateInvariant.
true.
%

doit
(TonelWriterTest
	subclass: 'TonelWriterForCypressTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #()
	inDictionary: Globals
	options: #()
)
		category: 'Tonel-Cypress-Tests';
		comment: '';
		immediateInvariant.
true.
%

! Class implementation for 'CypressTonelFileUrl'

!		Class methods for 'CypressTonelFileUrl'

category: 'constants'
classmethod: CypressTonelFileUrl
schemeName
  ^ 'tonel'
%

!		Instance methods for 'CypressTonelFileUrl'

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

! Class implementation for 'TonelCypressReader'

!		Class methods for 'TonelCypressReader'

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

!		Instance methods for 'TonelCypressReader'

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

! Class implementation for 'TonelCypressWriter'

!		Class methods for 'TonelCypressWriter'

category: 'instance creation'
classmethod: TonelCypressWriter
on: sourceDir
	^ self new
		sourceDir: sourceDir;
		yourself
%

!		Instance methods for 'TonelCypressWriter'

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

! Class implementation for 'CypressTonelRepository'

!		Instance methods for 'CypressTonelRepository'

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

category: 'reading'
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

! Class implementation for 'GsTonelOrderedDictionary'

!		Class methods for 'GsTonelOrderedDictionary'

category: 'instance creation'
classmethod: GsTonelOrderedDictionary
new
	^ self new: 3
%

category: 'instance creation'
classmethod: GsTonelOrderedDictionary
new: anInteger
	^ self basicNew initialize: anInteger; yourself
%

category: 'instance creation'
classmethod: GsTonelOrderedDictionary
withAll: aDictionary
	^ (self new: aDictionary size)
		addAll: aDictionary;
		yourself
%

!		Instance methods for 'GsTonelOrderedDictionary'

category: 'accessing'
method: GsTonelOrderedDictionary
add: anAssociation
	self at: anAssociation key put: anAssociation value.
	^ anAssociation
%

category: 'adding'
method: GsTonelOrderedDictionary
addAll: aDictionary
	aDictionary keysAndValuesDo: [ :key :value | self at: key put: value ].
	^ aDictionary
%

category: 'accessing'
method: GsTonelOrderedDictionary
associations
	"Answer a Collection containing the receiver's associations."

	| result |
	result := WriteStream on: (Array new: self size).
	self associationsDo: [ :assoc | result nextPut: assoc ].
	^ result contents
%

category: 'enumerating'
method: GsTonelOrderedDictionary
associationsDo: aBlock
	self keysAndValuesDo: [ :key :value | aBlock value: key -> value ]
%

category: 'accessing'
method: GsTonelOrderedDictionary
at: aKey
	"Answer the value associated with aKey. Raise an exception, if no such key is defined."

	^ self at: aKey ifAbsent: [ self errorKeyNotFound ]
%

category: 'accessing'
method: GsTonelOrderedDictionary
at: aKey ifAbsent: aBlock
	"Answer the value associated with aKey. Evaluate aBlock, if no such key is defined."

	| index |
	index := self findIndexFor: aKey.
	^ index = 0
		ifFalse: [ values at: index ]
		ifTrue: [ aBlock value ]
%

category: 'accessing'
method: GsTonelOrderedDictionary
at: aKey ifAbsentPut: aBlock
	"Answer the value associated with aKey. Evaluate aBlock, if no such key is defined and store the return value."

	| index |
	index := self findIndexFor: aKey.
	^ index = 0
		ifFalse: [ values at: index ]
		ifTrue: [ self privateAt: aKey put: aBlock value ]
%

category: 'accessing'
method: GsTonelOrderedDictionary
at: aKey ifPresent: aBlock
	"Lookup aKey in the receiver. If it is present, answer the value of evaluating the given block with the value associated with the key. Otherwise, answer nil."

	| index |
	index := self findIndexFor: aKey.
	^ index = 0 ifFalse: [ aBlock value: (values at: index) ]
%

category: 'accessing'
method: GsTonelOrderedDictionary
at: aKey put: aValue
	"Set the value of aKey to be aValue."

	| index |
	index := self findIndexFor: aKey.
	^ index = 0
		ifFalse: [ values at: index put: aValue ]
		ifTrue: [ self privateAt: aKey put: aValue ]
%

category: 'enumerating'
method: GsTonelOrderedDictionary
do: aBlock
	1 to: size do: [ :index | aBlock value: (values at: index) ]
%

category: 'private'
method: GsTonelOrderedDictionary
errorKeyNotFound
	self error: 'Key not found'
%

category: 'private'
method: GsTonelOrderedDictionary
findIndexFor: aKey
	1 to: size do: [ :index |
		(keys at: index) = aKey
			ifTrue: [ ^ index ] ].
	^ 0
%

category: 'private'
method: GsTonelOrderedDictionary
grow
	| newKeys newValues |
	newKeys := Array new: 2 * size.
	newValues := Array new: 2 * size.
	1 to: size do: [ :index |
		newKeys at: index put: (keys at: index).
		newValues at: index put: (values at: index) ].
	keys := newKeys.
	values := newValues
%

category: 'testing'
method: GsTonelOrderedDictionary
includesKey: aKey
	"Answer whether the receiver has a key equal to aKey."

	^ (self findIndexFor: aKey) ~= 0
%

category: 'initialization'
method: GsTonelOrderedDictionary
initialize: anInteger
  size := 0.
  keys := Array new: anInteger.
  values := Array new: anInteger
%

category: 'testing'
method: GsTonelOrderedDictionary
isCollection
	^ true
%

category: 'testing'
method: GsTonelOrderedDictionary
isEmpty
	^ size = 0
%

category: 'enumerating'
method: GsTonelOrderedDictionary
keys
	^ keys copyFrom: 1 to: size
%

category: 'enumerating'
method: GsTonelOrderedDictionary
keysAndValuesDo: aBlock
	1 to: size do: [ :index | aBlock value: (keys at: index) value: (values at: index) ]
%

category: 'enumerating'
method: GsTonelOrderedDictionary
keysDo: aBlock
	1 to: size do: [ :each | aBlock value: (keys at: each) ]
%

category: 'copying'
method: GsTonelOrderedDictionary
postCopy
	super postCopy.
	keys := keys copy.
	values := values copy
%

category: 'printing'
method: GsTonelOrderedDictionary
printOn: aStream
	super printOn: aStream.
	
	aStream nextPut: $(.
	self size <= 100
		ifTrue: [
			| first |
			first := true.
			self keysAndValuesDo: [ :key :value |
				"keysAndValuesDo:separatedBy: would be nice"
				first
					ifTrue: [ first := false ]
					ifFalse: [ aStream space ].
				aStream
					print: key;
					nextPutAll: '->';				
					print: value ] ]
		ifFalse: [
			aStream
				nextPutAll: 'size ';
				print: self size ].
	aStream nextPut: $)	
%

category: 'private'
method: GsTonelOrderedDictionary
privateAt: aKey put: aValue
	size = keys size ifTrue: [ self grow ].
	keys at: (size := size + 1) put: aKey.
	^ values at: size put: aValue
%

category: 'private'
method: GsTonelOrderedDictionary
removeIndex: index
	| value |
	value := values at: index.
	index to: size - 1 do:
			[ :i | 
			keys at: i put: (keys at: i + 1).
			values at: i put: (values at: i + 1) ].
	keys at: size put: nil.
	values at: size put: nil.
	size := size - 1.
	^ value
%

category: 'accessing'
method: GsTonelOrderedDictionary
removeKey: aKey
	"Remove aKey from the receiver, raise an exception if the element is missing."

	^ self removeKey: aKey ifAbsent: [ self errorKeyNotFound ]
%

category: 'accessing'
method: GsTonelOrderedDictionary
removeKey: aKey ifAbsent: aBlock
	"Remove aKey from the receiver, evaluate aBlock if the element is missing."

	| index |
	index := self findIndexFor: aKey.
	index = 0 ifTrue: [ ^ aBlock value ].
	^ self removeIndex: index
%

category: 'accessing'
method: GsTonelOrderedDictionary
size
	^ size
%

category: 'ston'
method: GsTonelOrderedDictionary
stonOn: stonWriter
	"Instances of STON mapClass will be encoded directly, without a class tag.
	Other (sub)classes will be encoded with a class tag and will use a map representation. "
	
    stonWriter encodeMap: self
%

category: 'enumerating'
method: GsTonelOrderedDictionary
values
	^ values copyFrom: 1 to: size
%

! Class implementation for 'TonelParser'

!		Class methods for 'TonelParser'

category: 'instance creation'
classmethod: TonelParser
on: aStream forReader: aTonelReader
	^ self new 
		stream: aStream;
		packageReader: aTonelReader;
		yourself
%

category: 'instance creation'
classmethod: TonelParser
onString: aString forReader: aTonelReader
  ^ self on: (self readStreamClass on: aString) forReader: aTonelReader
%

category: 'parsing'
classmethod: TonelParser
parseStream: aStream forReader: aTonelReader
	^ (self on: aStream forReader: aTonelReader)
		 start
%

category: 'parsing'
classmethod: TonelParser
parseString: aString forReader: aTonelReader
	^ self parseStream: (self readStreamClass on: aString) forReader: aTonelReader
%

category: 'Topaz support'
classmethod: TonelParser
_compileForTopaz: aString envId: envId
  "aString is the topaz text object for topaz TMETHOD command"
| strm parser catDict category methInfo methSrc symList clsName cls
  warnStr  |
strm :=  ReadStreamPortable on: aString  .
(parser := self new) stream: strm .

parser separator ifNotNil:[ Error signal:'expected parser separator to be nil'].

catDict :=  (parser try: [ parser metadata ]).
catDict ifNil:[ Error signal:'Category dictionary not found'. ].
category := catDict at: #category .

parser separator ifNotNil:[ Error signal:'expected second parser separator to be nil'].

methInfo :=  parser method.
clsName := Symbol _existingWithAll: ((methInfo at: 1) at: 1) .

methSrc :=  (methInfo at: 2)"keywords+args" ,  parser methodBody .
symList := System myUserProfile symbolList .
cls := (symList resolveSymbol: clsName) value .
((methInfo at: 1) at: 2) ifNotNil:[:classWord |
  classWord = 'class' ifTrue:[ cls := cls class ]
      ifFalse:[ Error signal:'unrecognized ' , classWord asString, ' after >>'].
].
[ cls compileMethod: methSrc dictionaries: symList 
               category: category  
               environmentId: envId .
] on: CompileWarning do:[:ex | 
   warnStr := ex warningString .
   ex resume 
].
^ warnStr "nil if no warnings"
%

!		Instance methods for 'TonelParser'

category: 'private'
method: TonelParser
cleanSelector: aString
	"BEWARE: I'm doing some heave assumptions here: I'm removing just ONE space (in case there 
	 is one) because I expect this to be a file generated by tonel, and tonel adds one space 
	 before start with the method body to make the format more readable. 
	 But of course this is not very good :("
	^ (aString last = Character space
		ifTrue: [ aString allButLast ]
		ifFalse: [ aString ]) 
		trimLeft
%

category: 'parsing'
method: TonelParser
comment
	| result ch eatNext |
	
	result := String new writeStreamPortable.

	eatNext := false.
	stream next = $" ifFalse: [ TonelParseError signal: 'Can''t parse comment' ].	
	[ stream atEnd not 
		and: [ 
				(ch := stream next) ~= $" 
				or: [ eatNext := (stream peek = $") ] ] ]
	whileTrue: [ 
		result nextPut: ch.
		eatNext ifTrue: [ 
			stream skip: 1.
			eatNext := false ] ].
	
	^ self 
		removeFrom: '"',result contents,'"' 
		enclosingStart: $" 
		end: $"
%

category: 'private factory'
method: TonelParser
definitionForType: aString
  ^ self packageReader definitionForType: aString
%

category: 'parsing'
method: TonelParser
document
	^ { 
	self typeDef.
	self methodDefList.
	 } 
	select: [:each | each notNil ]
%

category: 'private'
method: TonelParser
extractSelector: aString
	| separators selectorStream keywords |
	
	separators := { 
		Character space. 
		Character tab. 
		Character lf. 
		Character newPage. 
		Character cr. 
		$:}.

	keywords := Array new writeStreamPortable.
	selectorStream := aString readStream.
	[ selectorStream atEnd ]
	whileFalse: [ | word ch |
		word := String new writeStreamPortable.
		[ selectorStream atEnd not and: [ (separators includes: (ch := selectorStream next)) not ] ]
		whileTrue: [ word nextPut: ch ].
		ch = $: ifTrue: [ word nextPut: ch ]. 
		word contents trimBoth ifNotEmpty: [ :v | keywords nextPut: v ] ].
	keywords := keywords contents.

	^ (keywords size <= 2 
		ifTrue: [ keywords first]
		ifFalse: [ ('' join: (keywords pairsCollect: [ :keyword :argument | keyword ])) ])
		asSymbol
%

category: 'testing'
method: TonelParser
isEnter: aCharacter
	^ #(13 10) includes: aCharacter asciiValue
%

category: 'testing'
method: TonelParser
isSeparator: aCharacter 
	^ aCharacter isSeparator
%

category: 'parsing'
method: TonelParser
metadata
	| result ch count |
	
	result := String new writeStreamPortable.

	count := 0.
	stream peek = ${ ifFalse: [ TonelParseError signal: 'Can''t parse metadata' ].	
	[ stream atEnd not ]
	whileTrue: [ 
		ch := stream next.
		result nextPut: ch.
		ch = ${ ifTrue: [ count := count +1 ].
		ch = $} ifTrue: [ count := count -1 ].
		count = 0 ifTrue: [ ^ STON fromString: result contents ]].

	TonelParseError signal: 'Can''t parse metadata'
%

category: 'parsing'
method: TonelParser
method
	| type selector |
	
	type := self untilIncluding: '>>'.
	selector := self cleanSelector: (self untilExcluding: '[').
	type := type trimBoth substrings: ' '.
	type size = 1 ifTrue: [ type := type copyWith: nil ].
  lastSelectorParsed := selector .
	^ { 
		type.
		selector.
	}
%

category: 'parsing'
method: TonelParser
methodBody
	"I read a methodbody (what is inside [ ... ])
	 Since a method body can contain enclosing brackets we need to be sure we will skip them and
	 correctly read the method. For that, I have to take into account: 
		- I can mention [] in comments
		- I can mention [] in strings
		- I can use $[, $] 
		- I can have inner blocks
		- I can mention a comment of the form ""$"" or a comment of the form '$'
	 all that needs to be skipped "
	| result char prevChar comment string count startPos |
	
	result := self class writeStreamClass on: String new.

	comment := false.
	string := false.
	prevChar := nil.
	count := 0.
        startPos := stream position .
        "startBody := stream peek: 300 ." "uncomment for debugging parse problems"
	stream peek = $[ ifFalse: [ TonelParseError signal: 'Can''t parse method body' ].
	[ stream atEnd not ]
	whileTrue: [ 
		char := stream next.
		result nextPut: char.
		(char = $" and: [ string not and: [ prevChar ~= $$ or: [ comment ] ] ]) 
			ifTrue: [ comment := comment not ]. 
		(char = $' and: [ comment not and: [ prevChar ~= $$ or: [ string ] ] ]) 
			ifTrue: [ string := string not ]. 
		(comment or: [ string ]) ifFalse: [ 
			(char = $[ and: [  prevChar ~= $$ ]) ifTrue: [ count := count +1 ].
			(char = $] and: [ prevChar ~= $$ ]) ifTrue: [ count := count -1 ] ].
		count = 0 ifTrue: [ 
			^ self 
				removeFrom: result contents 
				enclosingStart: $[ 
				end: $]
				clean: #right ].
		prevChar := char ].

	TonelParseError signal: 'Can''t parse method body'
%

category: 'parsing'
method: TonelParser
methodDef
	"kept around for tests"

	| methodDef |
	self methodDef: [:isMeta :mDef |
		methodDef :=  mDef.
		"skip possible spaces at the end"
		self separator ].
	^methodDef
%

category: 'parsing'
method: TonelParser
methodDef: aBlock
	| ar def |
	ar := { 
		self separator.
		self try: [ self metadata ]. 
		self separator. 
		self method. 
		self methodBody 
	}.
	def := self newMethodDefinitionFrom: ar.
	aBlock 
		value: ar fourth first second notNil 
		value: def
%

category: 'parsing'
method: TonelParser
methodDefList
	| result classStream instanceStream |
	self separator. "to arrive to the end of the file in case there are no methods"
	result := { {}. {} }.
	classStream := (result at: 1) writeStreamPortable.
	instanceStream := (result at: 2) writeStreamPortable.
	[
		[ stream atEnd ]
			whileFalse: [ 
				self methodDef: [:isMeta :mDef |
					isMeta
						ifTrue: [ classStream nextPut: mDef ]
						ifFalse: [ instanceStream nextPut: mDef ].
					"skip possible spaces at the end"
					self separator ]
			] 
  ] on: TonelParseError do:[:ex | 
		lastSelectorParsed ifNotNil:[ | str |
      str := ex details ifNil:[ '' ].
      ex details: str, ', after tonel method selector: ', lastSelectorParsed printString 
    ].
		ex pass 
  ].
	^ result
%

category: 'private factory'
method: TonelParser
newMethodDefinitionFrom: anArray
	| metadata className meta selector source  |
	
	metadata := anArray second ifNil: [ Dictionary new ].
	className := anArray fourth first first.
	meta := anArray fourth first second notNil.
	selector := self extractSelector: anArray fourth second trimBoth.
	source := String streamContents: [ :s | 
		s << anArray fourth second.
		anArray fifth ifNotEmpty: [ :src | s << src ] ].

	^ self packageReader newMethodDefinitionForClassNamed: className
		classIsMeta: meta
		selector: selector
		category: (metadata at: #category ifAbsent: [ '' ]) 
		source: source
%

category: 'private factory'
method: TonelParser
newTypeDefinitionFrom: anArray
	^ self packageReader newTypeDefinitionFrom: anArray
%

category: 'accessing'
method: TonelParser
packageReader
	^ packageReader
%

category: 'accessing'
method: TonelParser
packageReader: aPackageReader 
	packageReader := aPackageReader
%

category: 'private'
method: TonelParser
removeFrom: aString enclosingStart: startChar end: endChar
	^ self 
		removeFrom: aString 
		enclosingStart: startChar 
		end: endChar
		clean: #both
%

category: 'private'
method: TonelParser
removeFrom: aString enclosingStart: startChar end: endChar clean: cleanSymbol
  "cleanSymbol can be #left, #rigth and #both"

  | result stop ch start end |
  result := self class readStreamClass on: aString trimBoth.
  result peek = startChar
    ifFalse: [ TonelParseError signal: 'I cannot remove enclosing start' ].
  result skip: 1.
  (#(#'both' #'left') includes: cleanSymbol)
    ifTrue: [ 
      stop := TonelWriter lineEnding size.
      [ stop > 0 and: [ self isSeparator: (ch := result peek) ] ]
        whileTrue: [ 
          (self isEnter: ch)
            ifTrue: [ stop := stop - 1 ].
          result skip: 1 ] ].
  start := result position.
  result setToEnd.
  result skip: -1.
  result peek = endChar
    ifFalse: [ TonelParseError signal: 'I cannot remove enclosing end' ].
  result skip: -1.
  (#(#'both' #'right') includes: cleanSymbol)
    ifTrue: [ 
      stop := TonelWriter lineEnding size.
      [ stop > 0 and: [ self isSeparator: (ch := result peek) ] ]
        whileTrue: [ 
          (self isEnter: ch)
            ifTrue: [ stop := stop - 1 ].
          result skip: -1 ] ].
  end := result position.
  ^ result originalContents copyFrom: start + 1 to: end + 1
%

category: 'parsing'
method: TonelParser
separator
	[ stream atEnd not and: [ self isSeparator: stream peek ] ]
	whileTrue: [ stream next ].
	^ nil
%

category: 'parsing'
method: TonelParser
shebang
	"look for a '#!' in first two character position and skip to next line if present"

	(stream peekFor: $#) ifFalse: [ ^ nil ].	
	(stream peekFor: $!) ifFalse: [ ^ nil ].
	^ stream  upTo: Character lf.
%

category: 'accessing'
method: TonelParser
start
	^ self document
%

category: 'accessing'
method: TonelParser
stream: aStream 
	stream := aStream
%

category: 'private parsing'
method: TonelParser
try: aBlock
	^ self 
		try: aBlock 
		onSuccess: [ :parsedValue | parsedValue ] 
		onFailure: [ nil ]
%

category: 'private parsing'
method: TonelParser
try: aBlock onFailure: failureBlock
	^ self 
		try: aBlock 
		onSuccess: [ :parsedValue |  parsedValue ] 
		onFailure: failureBlock
%

category: 'private parsing'
method: TonelParser
try: aBlock onSuccess: successBlock
	^ self 
		try: aBlock 
		onSuccess: successBlock 
		onFailure: [ nil ]
%

category: 'private parsing'
method: TonelParser
try: aBlock onSuccess: successBlock onFailure: failureBlock
	| pos |
	
	pos := stream position.
	[ ^ successBlock value: aBlock value ]
	on: TonelParseError 
	do: [ :e | 
		stream position: pos.
		^ failureBlock value ]. 
	
%

category: 'parsing'
method: TonelParser
type
	self try: [ self word: 'Class' ] onSuccess: [ :word | ^ word  ].
	self try: [ self word: 'Trait' ] onSuccess: [ :word | ^ word  ].
	self try: [ self word: 'Extension' ] onSuccess: [ :word | ^ word  ].
	
	"at end"
	TonelParseError signal: 'Can''t parse type.'	
%

category: 'parsing'
method: TonelParser
typeDef
	| shebang |
	shebang := self shebang. "ignore shebang on first line of file if present"
	^ self newTypeDefinitionFrom: { 
		self separator.
		self try: [ self comment ]. 
		self separator. 
		self type. 
		self separator. 
		self try: [ 
			| typeMetadata normalizedMetadata |
			typeMetadata := self metadata.
			normalizedMetadata := Dictionary new.
			typeMetadata keysAndValuesDo: [:key :value |
				normalizedMetadata at: key asLowercase asSymbol put: value ].
			normalizedMetadata at: #shebang put: shebang.
			normalizedMetadata ] 
	}
%

category: 'private parsing'
method: TonelParser
untilExcluding: aCollection
	| result |
	result := stream upToAll: aCollection.
	stream position: stream position - aCollection size.
	^ result
%

category: 'private parsing'
method: TonelParser
untilIncluding: aCollection
	^ stream upToAll: aCollection
%

category: 'private parsing'
method: TonelParser
word: aString
	| result |
	result := stream next: aString size.
	result = aString
		ifFalse: [ TonelParseError signal: 'Can''t parse ', aString ].
	^ result
%

! Class implementation for 'TonelWriter'

!		Class methods for 'TonelWriter'

category: 'accessing'
classmethod: TonelWriter
classLabel
	^ 'Class'
%

category: 'accessing'
classmethod: TonelWriter
extensionLabel
	^ 'Extension'
%

category: 'instance creation'
classmethod: TonelWriter
on: aPackageWriter

	^ self new
		packageWriter: aPackageWriter;
		yourself
%

category: 'accessing'
classmethod: TonelWriter
traitLabel
	^ 'Trait'
%

!		Instance methods for 'TonelWriter'

category: 'private'
method: TonelWriter
classNameFor: aMethodDefinition parent: aClassDefinition
	aClassDefinition ifNil: [ ^ aMethodDefinition fullClassName ].
	^ aMethodDefinition classIsMeta
		ifFalse: [ aMethodDefinition className ]
		ifTrue: [ 
			aClassDefinition isTraitDefinition
				ifFalse: [aMethodDefinition className, ' class']
				ifTrue: [aMethodDefinition className, ' classSide'] ]
%

category: 'private definitions'
method: TonelWriter
commentOf: aClassDefinition
	^ (aClassDefinition comment 
		copyReplaceAll: '"' 
		with: '""')
		withLineEndings: self newLine
%

category: 'accessing'
method: TonelWriter
definitions

   ^self packageWriter definitions
%

category: 'private'
method: TonelWriter
fileNameFor: aClassDefinition
	^ String streamContents: [ :stream | 
		stream 
			<< aClassDefinition className
			<< '.' << (self typeOf: aClassDefinition) asLowercase
			<< '.st'  ]
%

category: 'private testing'
method: TonelWriter
isClass: aClassDefinition
	^ self packageWriter isClass: aClassDefinition
%

category: 'private testing'
method: TonelWriter
isTrait: aClassDefinition
	^ self packageWriter isTrait: aClassDefinition
%

category: 'private definitions'
method: TonelWriter
methodDefinitionOf: aMethodDefinition
	^ self toSTON: (self class orderedDictionaryClass new 
		at: #category put: aMethodDefinition category asSymbol; 
		yourself)	
	
%

category: 'private'
method: TonelWriter
newLine
	 ^ self class lineEnding
%

category: 'accessing'
method: TonelWriter
packageWriter
  ^ packageWriter
%

category: 'accessing'
method: TonelWriter
packageWriter: anObject

   packageWriter := anObject
%

category: 'private'
method: TonelWriter
selectorIsComplete: keywords in: aString
	| start |
	
	start := 1.
	keywords do: [ :each | | index | 
		index := aString 
			findString: each 
			startingAt: start 
			caseSensitive: true.
		index = 0 ifTrue: [ ^ false ].
		start := index + each size ].
	^ true
%

category: 'private'
method: TonelWriter
skipComment: aStream
	"I assume I'm on top of the begining of a comment"
	aStream skip: 1.
	[ aStream atEnd not 
		and: [ aStream next ~= $" or: [ aStream peek = $" ] ] ]
	whileTrue.	
%

category: 'private'
method: TonelWriter
skipSeparators: aStream
	[ aStream peek isSeparator ]
	whileTrue: [ aStream skip: 1 ]. 
%

category: 'private'
method: TonelWriter
splitMethodSource: aMethodDefinition into: aBlock
	| keywords source declaration |
	
	keywords := aMethodDefinition selector asSymbol keywords.
	source := aMethodDefinition source readStream.
	"Skip spaces"
	(source peek isSeparator) ifTrue: [ self skipSeparators: source ].
	"Skip comments"
	(source peek = $") ifTrue: [ self skipComment: source ]. 
	"Parse declaration"
	declaration := String new writeStreamPortable.
	[ (self selectorIsComplete: keywords in: declaration originalContents) not 
		or: [ ':+-/\*~<>=@,%|&?!' includes: declaration contents trimRight last ] ]
	whileTrue: [ 
		"get separators"
		[ source atEnd not and: [ source peek isSeparator ] ]
			whileTrue: [ declaration nextPut: source next ].
		"take next word"
		[ source atEnd not and: [ source peek isSeparator not ] ]
			whileTrue: [ declaration nextPut: source next ] ].
	aBlock 
		value: (declaration contents trimLeft withLineEndings: self newLine)
		value: (source upToEnd withLineEndings: self newLine)
%

category: 'private'
method: TonelWriter
toSTON: anObject
	^ (String streamContents: [ :stream | 
		(TonelSTONWriter on: stream) nextPut: anObject ])
		withLineEndings: self newLine
%

category: 'private definitions'
method: TonelWriter
typeClassDefinitionOf: aClassDefinition
	| definition |
	
	definition := self class orderedDictionaryClass new 
		at: #name put: aClassDefinition className asSymbol; 
		at: #superclass put: aClassDefinition superclassName asSymbol;
		yourself.

	aClassDefinition type = #normal ifFalse: [ 
		definition at: #type put: aClassDefinition type ].
	
	aClassDefinition hasTraitComposition ifTrue: [ 
		definition at: #traits put: aClassDefinition traitCompositionString ].
	
	aClassDefinition hasClassTraitComposition ifTrue: [ 
		definition at: #classTraits put: aClassDefinition classTraitCompositionString ].
	
	(aClassDefinition instVarNames)
		ifNotEmpty: [ :vars | definition at: #instVars put: vars asArray ].

	(aClassDefinition classVarNames)
		ifNotEmpty: [ :vars | definition at: #classVars put: vars asArray ].
		
	((aClassDefinition poolDictionaries) collect: [:each | each asString])
		ifNotEmpty: [ :vars | definition at: #pools put: vars asArray ].
		
	(aClassDefinition classInstVarNames)
		ifNotEmpty: [ :vars | definition at: #classInstVars put: vars asArray ].

	(aClassDefinition gs_constraints)
		ifNotEmpty: [:gs_constraints | definition at: #'gs_constraints' put: gs_constraints asArray ].

	(aClassDefinition gs_options)
		ifNotEmpty: [:gs_options | definition at: #'gs_options' put: gs_options asArray ].

	(aClassDefinition gs_reservedOop)
		ifNotEmpty: [:gs_reservedOop | definition at: #'gs_reservedOop' put: gs_reservedOop asString ].

	definition 		
		at: #category put: aClassDefinition category asSymbol.
	
	^ self toSTON: definition
%

category: 'private definitions'
method: TonelWriter
typeDefinitionOf: aClassDefinition
	(self isTrait: aClassDefinition) 
		ifTrue: [ ^ self typeTraitDefinitionOf: aClassDefinition ].
	^ self typeClassDefinitionOf: aClassDefinition
%

category: 'private'
method: TonelWriter
typeOf: aClassDefinition
	(self isClass: aClassDefinition) ifTrue: [ ^ self class classLabel ].
	(self isTrait: aClassDefinition) ifTrue: [ ^ self class traitLabel ].

	TonelShouldIgnore signal
%

category: 'writing'
method: TonelWriter
writeClass: aClassDefinition
	[ 
		self packageWriter 
			writePackageFileNamed: (self fileNameFor: aClassDefinition) 
			do:  [ :aStream | 
				self writeClassDefinition: aClassDefinition on: aStream.
				self writeClassSideMethodDefinitions: aClassDefinition on: aStream.
				self writeInstanceSideMethodDefinitions: aClassDefinition on: aStream ] ]
	on: TonelShouldIgnore
	do: [ :e | self logCr: 'ignoring: ', aClassDefinition asString ]
%

category: 'private writing'
method: TonelWriter
writeClassDefinition: aClassDefinition on: aStream
	| nl |
	nl := self newLine.
	
	aClassDefinition hasComment 
		ifTrue: [ 
			aStream 
				<< '"' << nl
				<< (self commentOf: aClassDefinition) << nl
				<< '"' << nl ].
	aStream
		<< (self typeOf: aClassDefinition) 
		<< ' ' << (self typeDefinitionOf: aClassDefinition ) << nl
%

category: 'private writing'
method: TonelWriter
writeClassSideMethodDefinitions: aClassDefinition on: aStream
	((self definitions 
		select: [ :each | 
			each isMethodDefinition 
			and: [ each className = aClassDefinition className
			and: [ each classIsMeta ] ] ])
		sortWithBlock: [ :a :b | a selector _unicodeLessThan: b selector ])
		do: [ :each | 
			self writeMethodDefinition: each parent: aClassDefinition on: aStream ]
%

category: 'writing'
method: TonelWriter
writeExtensionMethods: methods className: className

	| nl |
	nl := self newLine.
	self packageWriter
		writePackageFileNamed: className , '.extension.st'
		do: [ :s | 
			s << 'Extension '
				<< (self toSTON: {(#'name' -> className asSymbol)} asDictionary) << nl.
			((methods select: [ :m | m classIsMeta not ])
				sortWithBlock: [ :a :b | a selector  _unicodeLessThan: b selector ])
				do: [ :each | self writeMethodDefinition: each on: s ].
			((methods select: [ :m | m classIsMeta ])
				sortWithBlock: [ :a :b | a selector  _unicodeLessThan: b selector ])
				do: [ :each | self writeMethodDefinition: each on: s ] ]
%

category: 'private writing'
method: TonelWriter
writeInstanceSideMethodDefinitions: aClassDefinition on: aStream
	((self definitions 
		select: [ :each | 
			each isMethodDefinition 
			and: [ each className = aClassDefinition className
			and: [ each classIsMeta not ] ] ])
		sortWithBlock: [ :a :b | a selector _unicodeLessThan: b selector ])
		do: [ :each | 
			self writeMethodDefinition: each parent: aClassDefinition on: aStream ]

	
	
%

category: 'private writing'
method: TonelWriter
writeMethodDefinition: aMethodDefinition on: aStream
	^ self 
		writeMethodDefinition: aMethodDefinition 
		parent: nil 
		on: aStream
%

category: 'private writing'
method: TonelWriter
writeMethodDefinition: aMethodDefinition parent: aClassDefinition on: aStream
	| nl |
	
	nl := self newLine.
	self 
		splitMethodSource: aMethodDefinition 
		into: [ :methodDeclaration :methodBody | | fullClassName |
			fullClassName := self classNameFor: aMethodDefinition parent: aClassDefinition.
			aStream 
				<< nl 
				<< (self methodDefinitionOf: aMethodDefinition) << nl 
				<< fullClassName << ' >> ' << methodDeclaration 
				<< ' [' << methodBody << nl << ']' << nl ]
%

category: 'writing'
method: TonelWriter
writePackage: packageName
	self packageWriter 
		writePackageFileNamed: 'package.st'
		do:  [ :aStream | self writePackage: packageName on: aStream ]
%

category: 'writing'
method: TonelWriter
writePackage: packageName on: aStream

	aStream 
		<< 'Package ' 
		<< (self toSTON: { #name ->  packageName asSymbol } asDictionary) 
		<< self  newLine
%

! Class implementation for 'TonelSTONWriter'

!		Instance methods for 'TonelSTONWriter'

category: 'accessing'
method: TonelSTONWriter
aliases
	^ aliases
%

category: 'accessing'
method: TonelSTONWriter
aliases: aDictionary
	aliases := aDictionary
%

category: 'private'
method: TonelSTONWriter
encodeKey: key value: value

	super encodeKey: key asSymbol
		value: (value isSymbol ifTrue: [value asString] ifFalse: [value])
%

category: 'writing'
method: TonelSTONWriter
encodeMap: pairs
	| first |
	first := true.
	writeStream nextPut: ${.
	pairs isEmpty
		ifTrue: [
			self prettyPrintSpace ]
		ifFalse: [
			self indentedDo: [
				pairs size = 1 
					ifTrue: [ self prettyPrintSpace ]
					ifFalse: [ self newlineIndent ].
				pairs keysAndValuesDo: [ :key :value |
					first 
						ifTrue: [ first := false ] 
						ifFalse: [ self mapElementSeparator ].
					self encodeKey: key value: value ] ].
				pairs size = 1 
					ifTrue: [ self prettyPrintSpace ]
					ifFalse: [ self newlineIndent ] ].
	writeStream nextPut: $}
%

category: 'initialization'
method: TonelSTONWriter
initialize
	super initialize.
	self prettyPrint: true.
	aliases := { TonelWriter orderedDictionaryClass -> nil } asDictionary
%

category: 'accessing'
method: TonelSTONWriter
stonNameFor: aClass
	^ self aliases 
		at: aClass 
		ifAbsent: [ aClass stonName ]
%

category: 'writing'
method: TonelSTONWriter
writeObject: anObject do: block
	(jsonMode and: [ anObject class ~= STON listClass and: [ anObject class ~= STON mapClass ] ])
		ifTrue: [ self error: 'wrong object class for JSON mode' ].
	self with: anObject do: [
		(self stonNameFor: anObject class) ifNotNil: [ :stonName | 
			writeStream nextPutAll: stonName.
			self prettyPrintSpace ].
		block value ]
%

! Class implementation for 'TonelAbstractTest'

!		Class methods for 'TonelAbstractTest'

category: 'Testing'
classmethod: TonelAbstractTest
isAbstract
	"Override to true if a TestCase subclass is Abstract and should not have
	TestCase instances built from it"

	^self sunitName = #TonelAbstractTest
%

category: 'private'
classmethod: TonelAbstractTest
mockCypressSnapshotSTON
  ^ 'CypressSnapshot {
	#definitions : [
		CypressClassDefinition {
			#name : ''MCMockASubclass'',
			#superclassName : ''MCMockClassA'',
			#category : ''MonticelloMocks'',
			#comment : '''',
			#instVarNames : [
				''x''
			],
			#classInstVarNames : [ ],
			#classVarNames : [
				''Y''
			],
			#poolDictionaryNames : [ ],
			#subclassType : #normal
		},
		CypressMethodDefinition {
			#classIsMeta : true,
			#source : ''initialize\n\n\tInitializationOrder := InitializationOrder\n\t\tifNil: [ -100 ] \"let the test fail\"\n\t\tifNotNil: [ InitializationOrder + 1.]'',
			#category : ''as yet unclassified'',
			#selector : ''initialize'',
			#className : ''MCMockASubclass''
		},
		CypressMethodDefinition {
			#classIsMeta : false,
			#source : ''variables\n\t^ x + Y + MCMockClassA'',
			#category : ''as yet unclassified'',
			#selector : ''variables'',
			#className : ''MCMockASubclass''
		},
		CypressMethodDefinition {
			#classIsMeta : false,
			#source : ''variables2\n\t^ ivar + CVar'',
			#category : ''as yet unclassified'',
			#selector : ''variables2'',
			#className : ''MCMockASubclass''
		},
		CypressClassDefinition {
			#name : ''MCMockClassA'',
			#superclassName : ''MCMock'',
			#category : ''MonticelloMocks'',
			#comment : ''This is a mock class. The Monticello tests manipulated it to simulate a developer modifying code in the image.'',
			#instVarNames : [
				''ivar''
			],
			#classInstVarNames : [ ],
			#classVarNames : [
				''CVar'',
				''InitializationOrder''
			],
			#poolDictionaryNames : [ ],
			#subclassType : #normal
		},
		CypressMethodDefinition {
			#classIsMeta : true,
			#source : ''cVar\n\t^ CVar'',
			#category : ''as yet unclassified'',
			#selector : ''cVar'',
			#className : ''MCMockClassA''
		},
		CypressMethodDefinition {
			#classIsMeta : true,
			#source : ''initializationOrder\n\n\t^ InitializationOrder'',
			#category : ''as yet unclassified'',
			#selector : ''initializationOrder'',
			#className : ''MCMockClassA''
		},
		CypressMethodDefinition {
			#classIsMeta : true,
			#source : ''initialize\n\tCVar := #initialized.\n\tInitializationOrder := 1.\n'',
			#category : ''as yet unclassified'',
			#selector : ''initialize'',
			#className : ''MCMockClassA''
		},
		CypressMethodDefinition {
			#classIsMeta : true,
			#source : ''one\n\n\t^ 1'',
			#category : ''as yet unclassified'',
			#selector : ''one'',
			#className : ''MCMockClassA''
		},
		CypressMethodDefinition {
			#classIsMeta : true,
			#source : ''touchCVar\n\tCVar := #touched'',
			#category : ''as yet unclassified'',
			#selector : ''touchCVar'',
			#className : ''MCMockClassA''
		},
		CypressMethodDefinition {
			#classIsMeta : false,
			#source : ''a\n\t^ \''a2\'''',
			#category : ''numeric'',
			#selector : ''a'',
			#className : ''MCMockClassA''
		},
		CypressMethodDefinition {
			#classIsMeta : false,
			#source : ''b\n\t^ \''b1\'''',
			#category : ''numeric'',
			#selector : ''b'',
			#className : ''MCMockClassA''
		},
		CypressMethodDefinition {
			#classIsMeta : false,
			#source : ''c\n\t^ \''c1\'''',
			#category : ''numeric'',
			#selector : ''c'',
			#className : ''MCMockClassA''
		},
		CypressMethodDefinition {
			#classIsMeta : false,
			#source : ''d\n\t^ \''d\'''',
			#category : ''numeric'',
			#selector : ''d'',
			#className : ''MCMockClassA''
		},
		CypressMethodDefinition {
			#classIsMeta : false,
			#source : ''falsehood\n\t^ false'',
			#category : ''boolean'',
			#selector : ''falsehood'',
			#className : ''MCMockClassA''
		},
		CypressMethodDefinition {
			#classIsMeta : false,
			#source : ''moreTruth\n\n\t^ true'',
			#category : ''boolean'',
			#selector : ''moreTruth'',
			#className : ''MCMockClassA''
		},
		CypressMethodDefinition {
			#classIsMeta : false,
			#source : ''one\n\t^ 1'',
			#category : ''numeric'',
			#selector : ''one'',
			#className : ''MCMockClassA''
		},
		CypressMethodDefinition {
			#classIsMeta : false,
			#source : ''q'',
			#category : ''drag\''n\''drop'',
			#selector : ''q'',
			#className : ''MCMockClassA''
		},
		CypressMethodDefinition {
			#classIsMeta : false,
			#source : ''truth\n\t^ true'',
			#category : ''boolean'',
			#selector : ''truth'',
			#className : ''MCMockClassA''
		},
		CypressMethodDefinition {
			#classIsMeta : false,
			#source : ''two\n\t^ 2'',
			#category : ''numeric'',
			#selector : ''two'',
			#className : ''MCMockClassA''
		},
		CypressClassDefinition {
			#name : ''MCMockClassB'',
			#superclassName : ''MCMock'',
			#category : ''MonticelloMocks'',
			#comment : ''This comment has a bang! Bang! Bang!'',
			#instVarNames : [
				''ivarb''
			],
			#classInstVarNames : [
				''ciVar''
			],
			#classVarNames : [
				''CVar''
			],
			#poolDictionaryNames : [
				''MCMockAPoolDictionary''
			],
			#subclassType : #normal
		},
		CypressClassDefinition {
			#name : ''MCMockClassD'',
			#superclassName : ''Object'',
			#category : ''MonticelloMocks'',
			#comment : '''',
			#instVarNames : [ ],
			#classInstVarNames : [ ],
			#classVarNames : [ ],
			#poolDictionaryNames : [ ],
			#subclassType : #normal
		},
		CypressMethodDefinition {
			#classIsMeta : false,
			#source : ''one\n\t^ 1'',
			#category : ''as yet unclassified'',
			#selector : ''one'',
			#className : ''MCMockClassD''
		},
		CypressClassDefinition {
			#name : ''MCMockClassE'',
			#superclassName : ''Object'',
			#category : ''MonticelloMocks'',
			#comment : '''',
			#instVarNames : [ ],
			#classInstVarNames : [ ],
			#classVarNames : [ ],
			#poolDictionaryNames : [ ],
			#subclassType : #variable
		},
		CypressMethodDefinition {
			#classIsMeta : true,
			#source : ''two\n\t^ 2'',
			#category : ''as yet unclassified'',
			#selector : ''two'',
			#className : ''MCMockClassE''
		},
		CypressClassDefinition {
			#name : ''MCMockClassF'',
			#superclassName : ''Object'',
			#category : ''MonticelloMocks'',
			#comment : '''',
			#instVarNames : [ ],
			#classInstVarNames : [ ],
			#classVarNames : [
				''Foo''
			],
			#poolDictionaryNames : [ ],
			#subclassType : #normal
		},
		CypressClassDefinition {
			#name : ''MCMockClassG'',
			#superclassName : ''Object'',
			#category : ''MonticelloMocks'',
			#comment : '''',
			#instVarNames : [ ],
			#classInstVarNames : [ ],
			#classVarNames : [ ],
			#poolDictionaryNames : [ ],
			#subclassType : #words
		},
		CypressClassDefinition {
			#name : ''MCMockClassH'',
			#superclassName : ''Object'',
			#category : ''MonticelloMocks'',
			#comment : '''',
			#instVarNames : [ ],
			#classInstVarNames : [ ],
			#classVarNames : [ ],
			#poolDictionaryNames : [ ],
			#subclassType : #bytes
		},
		CypressClassDefinition {
			#name : ''MCMockClassI'',
			#superclassName : ''Object'',
			#category : ''MonticelloMocks'',
			#comment : '''',
			#instVarNames : [ ],
			#classInstVarNames : [ ],
			#classVarNames : [ ],
			#poolDictionaryNames : [ ],
			#subclassType : #weak
		},
		CypressMethodDefinition {
			#classIsMeta : false,
			#source : ''mockClassExtension\n\n\t\"I change the protocol of this method to resolve the failing test: MCChangeNotificationTest >> testExtMethodModified. This test basically test that when we modified an extension method, the extended package is marked as \''modified\''. The problem is that Monticello treat differently a classic method from an extension method, and this only by checking if the protocol name start with a star. Therefore, if the protocol does not match the extending package name, the extending package name will never be notified, and the test will fail. \"'',
			#category : ''*MonticelloMocks'',
			#selector : ''mockClassExtension'',
			#className : ''MCSnapshotTest''
		}
	]
}'
%

category: 'private'
classmethod: TonelAbstractTest
mockMCSnapshotSTON
	^ 'MCSnapshot {
	#definitions : [
		MCOrganizationDefinition {
			#categories : [
				#MonticelloMocks
			]
		},
		MCClassDefinition {
			#name : #MCMockASubclass,
			#superclassName : #MCMockClassA,
			#variables : OrderedCollection [
				MCInstanceVariableDefinition {
					#name : ''x''
				},
				MCClassVariableDefinition {
					#name : ''Y''
				}
			],
			#category : #MonticelloMocks,
			#type : #normal,
			#comment : '''',
			#commentStamp : '''',
			#traitComposition : ''{}'',
			#classTraitComposition : ''{}''
		},
		MCMethodDefinition {
			#classIsMeta : true,
			#source : ''initialize\r\r\tInitializationOrder := InitializationOrder\r\t\tifNil: [ -100 ] "let the test fail"\r\t\tifNotNil: [ InitializationOrder + 1.]'',
			#category : #''as yet unclassified'',
			#selector : #initialize,
			#className : #MCMockASubclass
		},
		MCMethodDefinition {
			#classIsMeta : false,
			#source : ''variables\r\t^ x + Y + MCMockClassA'',
			#category : #''as yet unclassified'',
			#selector : #variables,
			#className : #MCMockASubclass
		},
		MCMethodDefinition {
			#classIsMeta : false,
			#source : ''variables2\r\t^ ivar + CVar'',
			#category : #''as yet unclassified'',
			#selector : #variables2,
			#className : #MCMockASubclass
		},
		MCClassDefinition {
			#name : #MCMockClassA,
			#superclassName : #MCMock,
			#variables : OrderedCollection [
				MCInstanceVariableDefinition {
					#name : ''ivar''
				},
				MCClassVariableDefinition {
					#name : ''CVar''
				},
				MCClassVariableDefinition {
					#name : ''InitializationOrder''
				}
			],
			#category : #MonticelloMocks,
			#type : #normal,
			#comment : ''This is a mock class. The Monticello tests manipulated it to simulate a developer modifying code in the image.'',
			#commentStamp : ''cwp 8/10/2003 16:43'',
			#traitComposition : ''{}'',
			#classTraitComposition : ''{}''
		},
		MCMethodDefinition {
			#classIsMeta : true,
			#source : ''cVar\r\t^ CVar'',
			#category : #''as yet unclassified'',
			#selector : #cVar,
			#className : #MCMockClassA
		},
		MCMethodDefinition {
			#classIsMeta : true,
			#source : ''initializationOrder\r\r\t^ InitializationOrder'',
			#category : #''as yet unclassified'',
			#selector : #initializationOrder,
			#className : #MCMockClassA
		},
		MCMethodDefinition {
			#classIsMeta : true,
			#source : ''initialize\r\tCVar := #initialized.\r\tInitializationOrder := 1.\r'',
			#category : #''as yet unclassified'',
			#selector : #initialize,
			#className : #MCMockClassA
		},
		MCMethodDefinition {
			#classIsMeta : true,
			#source : ''one\r\r\t^ 1'',
			#category : #''as yet unclassified'',
			#selector : #one,
			#className : #MCMockClassA
		},
		MCMethodDefinition {
			#classIsMeta : true,
			#source : ''touchCVar\r\tCVar := #touched'',
			#category : #''as yet unclassified'',
			#selector : #touchCVar,
			#className : #MCMockClassA
		},
		MCMethodDefinition {
			#classIsMeta : false,
			#source : ''a\r\t^ \''a2\'''',
			#category : #numeric,
			#selector : #a,
			#className : #MCMockClassA
		},
		MCMethodDefinition {
			#classIsMeta : false,
			#source : ''b\r\t^ \''b1\'''',
			#category : #numeric,
			#selector : #b,
			#className : #MCMockClassA
		},
		MCMethodDefinition {
			#classIsMeta : false,
			#source : ''c\r\t^ \''c1\'''',
			#category : #numeric,
			#selector : #c,
			#className : #MCMockClassA
		},
		MCMethodDefinition {
			#classIsMeta : false,
			#source : ''d\r\t^ \''d\'''',
			#category : #numeric,
			#selector : #d,
			#className : #MCMockClassA
		},
		MCMethodDefinition {
			#classIsMeta : false,
			#source : ''falsehood\r\t^ false'',
			#category : #boolean,
			#selector : #falsehood,
			#className : #MCMockClassA
		},
		MCMethodDefinition {
			#classIsMeta : false,
			#source : ''moreTruth\r\r\t^ true'',
			#category : #boolean,
			#selector : #moreTruth,
			#className : #MCMockClassA
		},
		MCMethodDefinition {
			#classIsMeta : false,
			#source : ''one\r\t^ 1'',
			#category : #numeric,
			#selector : #one,
			#className : #MCMockClassA
		},
		MCMethodDefinition {
			#classIsMeta : false,
			#source : ''q'',
			#category : #''drag\''n\''drop'',
			#selector : #q,
			#className : #MCMockClassA
		},
		MCMethodDefinition {
			#classIsMeta : false,
			#source : ''truth\r\t^ true'',
			#category : #boolean,
			#selector : #truth,
			#className : #MCMockClassA
		},
		MCMethodDefinition {
			#classIsMeta : false,
			#source : ''two\r\t^ 2'',
			#category : #numeric,
			#selector : #two,
			#className : #MCMockClassA
		},
		MCClassDefinition {
			#name : #MCMockClassB,
			#superclassName : #MCMock,
			#variables : OrderedCollection [
				MCInstanceVariableDefinition {
					#name : ''ivarb''
				},
				MCClassVariableDefinition {
					#name : ''CVar''
				},
				MCPoolImportDefinition {
					#name : ''MCMockAPoolDictionary''
				},
				MCClassInstanceVariableDefinition {
					#name : ''ciVar''
				}
			],
			#category : #MonticelloMocks,
			#type : #normal,
			#comment : ''This comment has a bang! Bang! Bang!'',
			#commentStamp : '''',
			#traitComposition : ''{}'',
			#classTraitComposition : ''{}''
		},
		MCClassDefinition {
			#name : #MCMockClassD,
			#superclassName : #Object,
			#variables : OrderedCollection [ ],
			#category : #MonticelloMocks,
			#type : #normal,
			#comment : '''',
			#commentStamp : '''',
			#traitComposition : ''{}'',
			#classTraitComposition : ''{}''
		},
		MCMethodDefinition {
			#classIsMeta : false,
			#source : ''one\r\t^ 1'',
			#category : #''as yet unclassified'',
			#selector : #one,
			#className : #MCMockClassD
		},
		MCClassDefinition {
			#name : #MCMockClassE,
			#superclassName : #Object,
			#variables : OrderedCollection [ ],
			#category : #MonticelloMocks,
			#type : #variable,
			#comment : '''',
			#commentStamp : '''',
			#traitComposition : ''{}'',
			#classTraitComposition : ''{}''
		},
		MCMethodDefinition {
			#classIsMeta : true,
			#source : ''two\r\t^ 2'',
			#category : #''as yet unclassified'',
			#selector : #two,
			#className : #MCMockClassE
		},
		MCClassDefinition {
			#name : #MCMockClassF,
			#superclassName : #Object,
			#variables : OrderedCollection [
				MCClassVariableDefinition {
					#name : ''Foo''
				}
			],
			#category : #MonticelloMocks,
			#type : #normal,
			#comment : '''',
			#commentStamp : '''',
			#traitComposition : ''{}'',
			#classTraitComposition : ''{}''
		},
		MCClassDefinition {
			#name : #MCMockClassG,
			#superclassName : #Object,
			#variables : OrderedCollection [ ],
			#category : #MonticelloMocks,
			#type : #words,
			#comment : '''',
			#commentStamp : '''',
			#traitComposition : ''{}'',
			#classTraitComposition : ''{}''
		},
		MCClassDefinition {
			#name : #MCMockClassH,
			#superclassName : #Object,
			#variables : OrderedCollection [ ],
			#category : #MonticelloMocks,
			#type : #bytes,
			#comment : '''',
			#commentStamp : '''',
			#traitComposition : ''{}'',
			#classTraitComposition : ''{}''
		},
		MCClassDefinition {
			#name : #MCMockClassI,
			#superclassName : #Object,
			#variables : OrderedCollection [ ],
			#category : #MonticelloMocks,
			#type : #weak,
			#comment : '''',
			#commentStamp : '''',
			#traitComposition : ''{}'',
			#classTraitComposition : ''{}''
		},
		MCMethodDefinition {
			#classIsMeta : false,
			#source : ''mockClassExtension\r\r\t"I change the protocol of this method to resolve the failing test: MCChangeNotificationTest >> testExtMethodModified. This test basically test that when we modified an extension method, the extended package is marked as \''modified\''. The problem is that Monticello treat differently a classic method from an extension method, and this only by checking if the protocol name start with a star. Therefore, if the protocol does not match the extending package name, the extending package name will never be notified, and the test will fail. " '',
			#category : #''*MonticelloMocks'',
			#selector : #mockClassExtension,
			#className : #MCSnapshotTest
		}
	]
}'
%

!		Instance methods for 'TonelAbstractTest'

category: 'private'
method: TonelAbstractTest
fileUtils
  self subclassResponsibility
%

category: 'mocks'
method: TonelAbstractTest
mockCypressSnapshot
  ^ STON fromString: self class mockCypressSnapshotSTON
%

category: 'mocks'
method: TonelAbstractTest
mockMCSnapshot
	^ STON fromString: self class mockMCSnapshotSTON
%

! Class implementation for 'TonelAbstractWriterTest'

!		Class methods for 'TonelAbstractWriterTest'

category: 'Testing'
classmethod: TonelAbstractWriterTest
isAbstract
	"Override to true if a TestCase subclass is Abstract and should not have
	TestCase instances built from it"

	^self sunitName = #TonelAbstractWriterTest
%

!		Instance methods for 'TonelAbstractWriterTest'

category: 'tests'
method: TonelAbstractWriterTest
expectedMCMockASubclassClassSt
	^ 'Class {
	#name : ''MCMockASubclass'',
	#superclass : ''MCMockClassA'',
	#instVars : [
		''x''
	],
	#classVars : [
		''Y''
	],
	#category : ''MonticelloMocks''
}

{ #category : ''as yet unclassified'' }
MCMockASubclass class >> initialize [

	InitializationOrder := InitializationOrder
		ifNil: [ -100 ] "let the test fail"
		ifNotNil: [ InitializationOrder + 1.]
]

{ #category : ''as yet unclassified'' }
MCMockASubclass >> variables [
	^ x + Y + MCMockClassA
]

{ #category : ''as yet unclassified'' }
MCMockASubclass >> variables2 [
	^ ivar + CVar
]
'
%

category: 'tests'
method: TonelAbstractWriterTest
expectedMCMockClassAClassSt
	^ '"
This is a mock class. The Monticello tests manipulated it to simulate a developer modifying code in the image.
"
Class {
	#name : ''MCMockClassA'',
	#superclass : ''MCMock'',
	#instVars : [
		''ivar''
	],
	#classVars : [
		''CVar'',
		''InitializationOrder''
	],
	#category : ''MonticelloMocks''
}

{ #category : ''as yet unclassified'' }
MCMockClassA class >> cVar [
	^ CVar
]

{ #category : ''as yet unclassified'' }
MCMockClassA class >> initializationOrder [

	^ InitializationOrder
]

{ #category : ''as yet unclassified'' }
MCMockClassA class >> initialize [
	CVar := #initialized.
	InitializationOrder := 1.

]

{ #category : ''as yet unclassified'' }
MCMockClassA class >> one [

	^ 1
]

{ #category : ''as yet unclassified'' }
MCMockClassA class >> touchCVar [
	CVar := #touched
]

{ #category : ''numeric'' }
MCMockClassA >> a [
	^ ''a2''
]

{ #category : ''numeric'' }
MCMockClassA >> b [
	^ ''b1''
]

{ #category : ''numeric'' }
MCMockClassA >> c [
	^ ''c1''
]

{ #category : ''numeric'' }
MCMockClassA >> d [
	^ ''d''
]

{ #category : ''boolean'' }
MCMockClassA >> falsehood [
	^ false
]

{ #category : ''boolean'' }
MCMockClassA >> moreTruth [

	^ true
]

{ #category : ''numeric'' }
MCMockClassA >> one [
	^ 1
]

{ #category : ''drag\''n\''drop'' }
MCMockClassA >> q [
]

{ #category : ''boolean'' }
MCMockClassA >> truth [
	^ true
]

{ #category : ''numeric'' }
MCMockClassA >> two [
	^ 2
]
'
%

category: 'tests'
method: TonelAbstractWriterTest
expectedMCSnapshotTestExtensionSt
	^ 'Extension { #name : ''MCSnapshotTest'' }

{ #category : ''*MonticelloMocks'' }
MCSnapshotTest >> mockClassExtension [

	"I change the protocol of this method to resolve the failing test: MCChangeNotificationTest >> testExtMethodModified. This test basically test that when we modified an extension method, the extended package is marked as ''modified''. The problem is that Monticello treat differently a classic method from an extension method, and this only by checking if the protocol name start with a star. Therefore, if the protocol does not match the extending package name, the extending package name will never be notified, and the test will fail. "
]
'
%

category: 'mocks'
method: TonelAbstractWriterTest
mockSnapshot

	self subclassResponsibility
%

category: 'running'
method: TonelAbstractWriterTest
setUp
  directory := nil.
  super setUp
%

category: 'tests'
method: TonelAbstractWriterTest
testWriteSnapshot
  | writer dir nl packageDir snapshot |
  dir := self directory.
  writer := self writerClass on: dir.
  snapshot := self mockSnapshot.
  snapshot dynamicInstVarAt: #'packageName' put: 'MonticelloMocks'.
  writer writeSnapshot: snapshot.
  self assert: (self directoryNamed: 'MonticelloMocks' existsIn: dir).
  packageDir := self directoryNamed: 'MonticelloMocks' in: dir.
  self
    assert: (self fileNamesIn: packageDir)
    equals:
      #('MCMockASubclass.class.st' 'MCMockClassA.class.st' 'MCMockClassB.class.st' 'MCMockClassD.class.st' 'MCMockClassE.class.st' 'MCMockClassF.class.st' 'MCMockClassG.class.st' 'MCMockClassH.class.st' 'MCMockClassI.class.st' 'MCSnapshotTest.extension.st' 'package.st').
  nl := TonelWriter lineEnding.
  self
    assert:
      (self contentsOfFileNamed: 'MCMockClassA.class.st' inDirectory: packageDir)
    equals: (self expectedMCMockClassAClassSt withLineEndings: nl).
  self
    assert:
      (self contentsOfFileNamed: 'MCMockASubclass.class.st' inDirectory: packageDir)
    equals: (self expectedMCMockASubclassClassSt withLineEndings: nl).
  self
    assert:
      (self contentsOfFileNamed: 'MCSnapshotTest.extension.st' inDirectory: packageDir)
    equals: (self expectedMCSnapshotTestExtensionSt withLineEndings: nl)
%

category: 'private'
method: TonelAbstractWriterTest
writerClass

  self subclassResponsibility
%

! Class implementation for 'TonelCypressWriterTest'

!		Instance methods for 'TonelCypressWriterTest'

category: 'private'
method: TonelCypressWriterTest
fileUtils
  ^ CypressFileUtilities current
%

category: 'mocks'
method: TonelCypressWriterTest
mockSnapshot

	^ self mockCypressSnapshot
%

category: 'private'
method: TonelCypressWriterTest
writerClass

  ^ TonelCypressWriter
%

! Class implementation for 'TonelReaderTest'

!		Class methods for 'TonelReaderTest'

category: 'Testing'
classmethod: TonelReaderTest
isAbstract
	"Override to true if a TestCase subclass is Abstract and should not have
	TestCase instances built from it"

	^self sunitName = #TonelReaderTest
%

!		Instance methods for 'TonelReaderTest'

category: 'tests'
method: TonelReaderTest
assertClassDefinition: a and: b
	self assert: a className equals: b className.
	self assert: a superclassName equals: b superclassName.
	self assert: a traitCompositionString equals: b traitCompositionString.
	self assert: a classTraitCompositionString equals: b classTraitCompositionString.
	self assert: a category equals: b category.	
	self assert: a instVarNames equals: b instVarNames.
	self assert: a classInstVarNames equals: b classInstVarNames.
	self assert: a classVarNames equals: b classVarNames.
	self assert: a poolDictionaries equals: b poolDictionaries.
	self assert: a type equals: b type.
	self assert: a comment equals: b comment.
%

category: 'tests'
method: TonelReaderTest
assertDefinition: a and: b
	a isOrganizationDefinition ifTrue: [ ^ self assertOrganisationDefinition: a and: b ].
	a isClassDefinition ifTrue: [ ^ self assertClassDefinition: a and: b ].
	a isMethodDefinition ifTrue: [ ^ self assertMethodDefinition: a and: b ].
%

category: 'tests'
method: TonelReaderTest
assertMethodDefinition: a and: b
	self assert: a className equals: b className.
	self assert: a selector equals: b selector.
	self assert: a protocol equals: b protocol.
	self assert: a source asByteArray equals: b source asByteArray.
	self assert: a classIsMeta equals: b classIsMeta
%

category: 'tests'
method: TonelReaderTest
assertOrganisationDefinition: a and: b
	self assert: a categories equals: b categories
%

category: 'mocks'
method: TonelReaderTest
mockSnapshot

	self subclassResponsibility
%

category: 'tests'
method: TonelReaderTest
testLoadDefinitions
  | snapshot reader |
true ifTrue: [ ^ self ]. "with https://github.com/GemTalk/Rowan/issues/361 the Tonel/Cypress implementation will be phased and to get this test to pass, we have to arrange for the directory to be correctly passed into the TonelCypressReader ..."
  snapshot := self mockSnapshot.
  reader := self createReaderFor: snapshot fileName: 'MonticelloMocks'.
  reader loadDefinitions.
  self assert: reader definitions size equals: snapshot definitions size.
  reader definitions sorted
    with: snapshot definitions sorted
    do: [ :a :b | self assertDefinition: a and: b ]
%

! Class implementation for 'TonelCypressReaderTest'

!		Instance methods for 'TonelCypressReaderTest'

category: 'tests'
method: TonelCypressReaderTest
assertClassDefinition: a and: b
	self assert: a className equals: b className.
	self assert: a superclassName equals: b superclassName.
	self assert: a category equals: b category.	
	self assert: a instVarNames equals: b instVarNames.
	self assert: a classInstVarNames equals: b classInstVarNames.
	self assert: a classVarNames equals: b classVarNames.
	self assert: a poolDictionaries equals: b poolDictionaries.
	self assert: a type equals: b type.
	self assert: a comment equals: b comment.
%

category: 'tests'
method: TonelCypressReaderTest
assertDefinition: a and: b
	a isClassDefinition ifTrue: [ ^ self assertClassDefinition: a and: b ].
	a isMethodDefinition ifTrue: [ ^ self assertMethodDefinition: a and: b ].
%

category: 'tests'
method: TonelCypressReaderTest
assertMethodDefinition: a and: b
	self assert: a className equals: b className.
	self assert: a selector equals: b selector.
	self assert: a category equals: b category.
	self assert: a source asByteArray equals: b source asByteArray.
	self assert: a classIsMeta equals: b classIsMeta
%

category: 'private'
method: TonelCypressReaderTest
fileUtils
  ^ CypressFileUtilities current
%

category: 'mocks'
method: TonelCypressReaderTest
mockSnapshot
  ^ self mockCypressSnapshot
%

! Class implementation for 'TonelParserTest'

!		Class methods for 'TonelParserTest'

category: 'Testing'
classmethod: TonelParserTest
isAbstract
  "Override to true if a TestCase subclass is Abstract and should not have
	TestCase instances built from it"

  ^ self sunitName = #'TonelParserTest'
%

!		Instance methods for 'TonelParserTest'

category: 'asserting'
method: TonelParserTest
assertParse: aString rule: rule equals: result 
	self 
		assert: (self parse: aString rule: rule)
		equals: result
%

category: 'private'
method: TonelParserTest
newClassDefinitionForClassNamed: nameString superclassName: superclassString category: categoryString instVarNames: ivarArray classVarNames: cvarArray poolDictionaryNames: poolArray classInstVarNames: civarArray type: typeSymbol comment: commentString
  self subclassResponsibility
%

category: 'private'
method: TonelParserTest
newClassDefinitionFrom: anArray
  self subclassResponsibility
%

category: 'private'
method: TonelParserTest
newMethodDefinitionForClassNamed: className classIsMeta: meta selector: selector category: category source: source
  self subclassResponsibility
%

category: 'private'
method: TonelParserTest
newTraitDefinitionFrom: anArray
  self subclassResponsibility
%

category: 'private'
method: TonelParserTest
newTypeDefinitionFrom: anArray
  self subclassResponsibility
%

category: 'private'
method: TonelParserTest
parse: aString rule: rule
  | parser |
  parser := TonelParser onString: aString forReader: self.
  ^ parser perform: rule
%

category: 'asserting'
method: TonelParserTest
shouldParse: aString rule: rule raise: error
  | parser |
  parser := TonelParser onString: aString  forReader: self.
  self should: [ parser perform: rule ] raise: error
%

category: 'tests'
method: TonelParserTest
testComment
	self 
		shouldParse: 'this "should" be an error'
		rule: #comment
		raise: TonelParseError.
	
	self  
		assertParse: '"this is a comment"'
		rule: #comment
		equals: 'this is a comment'.
	
	self  
		assertParse: '"""this"" is a comment with ""nested"" colons ""also at the end"""'
		rule: #comment
		equals: '"this" is a comment with "nested" colons "also at the end"'	
		
		
%

category: 'tests'
method: TonelParserTest
testExtractSelector
	| parser |
	
	parser := TonelParser new.
	
	self assert: (parser extractSelector: 'unary') equals: #unary.
	self assert: (parser extractSelector: '+ something') equals: #+.
	self assert: (parser extractSelector: '==> other') equals: #==>.
	self 
		assert: (parser extractSelector: 'some: arg1 keyword: arg2 selector: arg3') 
		equals: #some:keyword:selector:.
	self 
		assert: (parser extractSelector: 'some: 	arg1 keyword:arg2 selector: arg3') 
		equals: #some:keyword:selector:.
	self 
		assert: (parser extractSelector: 'some: arg1 
keyword: arg2 
selector: arg3') 
		equals: #some:keyword:selector:.
%

category: 'tests'
method: TonelParserTest
testMetadata
	self 
		assertParse: '{ #vars: [ #a, #b ] }' 
		rule: #metadata 
		equals: { #vars -> #(a b) } asDictionary.
	
	self 
		assertParse: '{ 
	#vars: [ #a, #b ],
	#uses: { #someNested: 42 } 
	}' 
		rule: #metadata 
		equals: { 
			#vars -> #(a b). 
			#uses -> { #someNested -> 42 } asDictionary
		} asDictionary
%

category: 'tests'
method: TonelParserTest
testMethod
	self 
		assertParse: 'Object>>name' 
		rule: #method
		equals: #(('Object' nil) 'name').
		
	self 
		assertParse: 'Object >> name: aString' 
		rule: #method
		equals: #(('Object' nil) 'name: aString').
		
	self 
		assertParse: 'Object >> name: aString [ I do not care ]' 
		rule: #method
		equals: #(('Object' nil) 'name: aString').
		
	self 
		assertParse: 'Object class >> name: aString' 
		rule: #method
		equals: #(('Object' 'class') 'name: aString').
%

category: 'tests'
method: TonelParserTest
testMethodBody
	self 
		assertParse: '[ method body... I will ignore what is inside ]'
		rule: #methodBody
		equals: ' method body... I will ignore what is inside'.

	self 
		assertParse: '[
method 
	[body... [I 
		will ignore] 
what] is inside
]'
		rule: #methodBody
		equals: '
method 
	[body... [I 
		will ignore] 
what] is inside'.
		
	self 
		assertParse: '[ method body with "''", ''"'', "[", '']'' ]'
		rule: #methodBody
		equals: ' method body with "''", ''"'', "[", '']'''.
	
%

category: 'tests'
method: TonelParserTest
testMethodDef
	self 
		assertParse: '
{ #category: ''accessing'' }
Object>>name [
	^ self printString
]'
		rule: #methodDef
		equals: (self newMethodDefinitionForClassNamed: #Object
			classIsMeta: false
			selector: #name
			category: 'accessing' 
			source: 'name
	^ self printString').
			
	self 
		assertParse: '
Object class>>name [ 
	^ self printString
]'
		rule: #methodDef
		equals: (self newMethodDefinitionForClassNamed: #Object
			classIsMeta: true
			selector: #name
			category: ''
			source: 'name 
	^ self printString').

	self 
		assertParse: '
TClass classSide >> template: aSystemCategoryName [ 
	"I really do not care"
]'
		rule: #methodDef
		equals: (self newMethodDefinitionForClassNamed: #TClass
			classIsMeta: true
			selector: #template:
			category: ''
			source: 'template: aSystemCategoryName 
	"I really do not care"').
%

category: 'tests'
method: TonelParserTest
testMethodDefList
	| parsed |
	
	parsed := self 
		parse: '
Object class>>new [
	^ self basicNew initialize
]

{ #category: ''accessing'' }
Object>>name [
	^ self printString
]

{ #category: ''printing'' }
Object>>printOn: aStream [
	"Append to the argument, aStream, a sequence of characters that  
	identifies the receiver."

	| title |
	title := self class name.
	aStream
		nextPutAll: (title first isVowel ifTrue: [''an ''] ifFalse: [''a '']);
		nextPutAll: title

]'
		rule: #methodDefList.
		
	self assert: parsed flattened size equals: 3
%

category: 'tests'
method: TonelParserTest
testRemoveFromEnclosingStartEnd
  | parser nl |
  nl := TonelWriter lineEnding.
  parser := TonelParser new.
  self
    assert: (parser removeFrom: '[ ^ self ]' enclosingStart: $[ end: $])
    equals: '^ self'.
  self
    assert:
      (parser
        removeFrom:
          ('[ 
	^ self ]' withLineEndings: nl)
        enclosingStart: $[
        end: $])
    equals: Character tab asString , '^ self'.
  self
    assert:
      (parser
        removeFrom:
          ('[ 
	^ self
	]' withLineEndings: nl)
        enclosingStart: $[
        end: $])
    equals: Character tab asString , '^ self'.
  self
    assert:
      (parser
        removeFrom:
          ('[ 

	^ self

]' withLineEndings: nl)
        enclosingStart: $[
        end: $])
    equals:
      ('
	^ self
' withLineEndings: nl)
%

category: 'tests'
method: TonelParserTest
testType
	self assertParse: 'Class' rule: #type equals: 'Class'.
	self assertParse: 'Trait' rule: #type equals: 'Trait'.
	self assertParse: 'Extension' rule: #type equals: 'Extension'
%

category: 'tests'
method: TonelParserTest
testTypeDef
  self
    assertParse:
      '
"
this is a test
"
Class { 
	#name: ''X'',
	#superclass: ''Y'',
	#category: ''Z'' 
}'
    rule: #'typeDef'
    equals:
      (self
        newClassDefinitionForClassNamed: 'X'
        superclassName: 'Y'
        category: 'Z'
        instVarNames: #()
        classVarNames: #()
        poolDictionaryNames: #()
        classInstVarNames: #()
        type: #'normal'
        comment: 'this is a test')
%

category: 'tests'
method: TonelParserTest
testTypeDefWithClassVars
  self
    assertParse:
      '
"
this is a test
"
Class {
	#name : #MCMockASubclass,
	#superclass : #MCMockClassA,
	#instVars : [
		''x''
	],
	#classVars : [
		''Y''
	],
	#category : #MonticelloMocks
}
'
    rule: #'typeDef'
    equals:
      (self
        newClassDefinitionForClassNamed: 'MCMockASubclass'
        superclassName: 'MCMockClassA'
        category: 'MonticelloMocks'
        instVarNames: #(#'x')
        classVarNames: #(#'Y')
        poolDictionaryNames: #()
        classInstVarNames: #()
        type: #'normal'
        comment: 'this is a test')
%

! Class implementation for 'TonelParserForCypressTest'

!		Instance methods for 'TonelParserForCypressTest'

category: 'private'
method: TonelParserForCypressTest
newClassDefinitionForClassNamed: nameString superclassName: superclassString category: categoryString instVarNames: ivarArray classVarNames: cvarArray poolDictionaryNames: poolArray classInstVarNames: civarArray type: typeSymbol comment: commentString
  ^ TonelCypressReader
    newClassDefinitionForClassNamed: nameString
    superclassName: superclassString
    category: categoryString
    instVarNames: ivarArray
    classVarNames: cvarArray
    poolDictionaryNames: poolArray
    classInstVarNames: civarArray
    type: typeSymbol
    comment: commentString
%

category: 'private'
method: TonelParserForCypressTest
newClassDefinitionFrom: anArray
  ^ TonelCypressReader newClassDefinitionFrom: anArray
%

category: 'private'
method: TonelParserForCypressTest
newMethodDefinitionForClassNamed: className classIsMeta: meta selector: selector category: category source: source
  ^ TonelCypressReader
    newMethodDefinitionForClassNamed: className
    classIsMeta: meta
    selector: selector
    category: category
    source: source
%

category: 'private'
method: TonelParserForCypressTest
newTraitDefinitionFrom: anArray
  ^ TonelCypressReader newTraitDefinitionFrom: anArray
%

category: 'private'
method: TonelParserForCypressTest
newTypeDefinitionFrom: anArray
  ^ TonelCypressReader newTypeDefinitionFrom: anArray
%

! Class implementation for 'TonelWriterTest'

!		Class methods for 'TonelWriterTest'

category: 'Testing'
classmethod: TonelWriterTest
isAbstract
  "Override to true if a TestCase subclass is Abstract and should not have
	TestCase instances built from it"

  ^ self sunitName = #'TonelWriterTest'
%

!		Instance methods for 'TonelWriterTest'

category: 'private'
method: TonelWriterTest
creatClassDefinition: nameString superclassName: superclassString category: categoryString instVarNames: ivarArray classVarNames: cvarArray poolDictionaryNames: poolArray classInstVarNames: civarArray type: typeSymbol comment: commentString
  self subclassResponsibility
%

category: 'private'
method: TonelWriterTest
creatClassDefinition: nameString superclassName: superclassString traitComposition: traitCompositionString classTraitComposition: classTraitCompositionString category: categoryString instVarNames: ivarArray classVarNames: cvarArray poolDictionaryNames: poolArray classInstVarNames: civarArray type: typeSymbol comment: commentString
  self subclassResponsibility
%

category: 'private'
method: TonelWriterTest
creatMethodDefinition: classString classIsMeta: metaBoolean selector: selectorString category: catString source: sourceString
  self subclassResponsibility
%

category: 'private'
method: TonelWriterTest
creatMethodDefinition: classString selector: selectorString category: catString source: sourceString
  ^ self
    creatMethodDefinition: classString
    classIsMeta: false
    selector: selectorString
    category: catString
    source: sourceString
%

category: 'private'
method: TonelWriterTest
defaultPackageWriter
  self subclassResponsibility
%

category: 'tests'
method: TonelWriterTest
testSplitMethodSourceInto
  | writer declaration source definition newLine tab space |
  newLine := TonelWriter lineEnding.
  tab := Character tab asString.
  space := Character space asString.
  writer := TonelWriter new.	"simplest split"
  definition := self creatMethodDefinition: #'SomeClass'
    selector: #'name'
    category: 'accessing'
    source:
      'name
	^ self'.
  writer
    splitMethodSource: definition
    into: [ :d :s | 
      declaration := d.
      source := s ].
  self assert: declaration equals: 'name'.
  self assert: source equals: newLine , tab , '^ self'.	"test space at the end of method declaration (it needs to be kept)"
  definition := self creatMethodDefinition: #'SomeClass'
    selector: #'name:'
    category: 'accessing'
    source:
      'name: aString 
	name := aString'.
  writer
    splitMethodSource: definition
    into: [ :d :s | 
      declaration := d.
      source := s ].
  self assert: declaration equals: 'name: aString'.
  self assert: source equals: space , newLine , tab , 'name := aString'.	"test multiline declaration"
  definition := self creatMethodDefinition: #'SomeClass'
    selector: #'method:with:several:lines:'
    category: 'accessing'
    source:
      'method: var1
	with: var2
	several: var3
	lines: var4
	
	^ var1 + var2 + var3 + var4'.
  writer
    splitMethodSource: definition
    into: [ :d :s | 
      declaration := d.
      source := s ].
  self
    assert: declaration
    equals:
      ('method: var1
	with: var2
	several: var3
	lines: var4'
        withLineEndings: TonelWriter lineEnding).
  self
    assert: source
    equals:
      (newLine , tab , newLine , tab , '^ var1 + var2 + var3 + var4'
        withLineEndings: newLine).	"test comment before declaration (it may happen, if someone copied from diffmorph)"
  definition := self creatMethodDefinition: #'SomeClass'
    selector: #'name:'
    category: 'accessing'
    source:
      '
"protocol: accessing"
name: aString 
	name := aString'.
  writer
    splitMethodSource: definition
    into: [ :d :s | 
      declaration := d.
      source := s ].
  self assert: declaration equals: 'name: aString'.
  self assert: source equals: space , newLine , tab , 'name := aString'.	"test source right after declaration (no enter between selector and source)"
  definition := self creatMethodDefinition: #'SomeClass'
    selector: #'name:'
    category: 'accessing'
    source: 'name: aString name := aString'.
  writer
    splitMethodSource: definition
    into: [ :d :s | 
      declaration := d.
      source := s ].
  self assert: declaration equals: 'name: aString'.
  self assert: source equals: ' name := aString'.	"test method name containin name of keywords in variables"
  definition := self creatMethodDefinition: #'SomeClass'
    selector: #'a:b:c:'
    category: 'accessing'
    source: 'a: b b: c c: d ^ 42'.
  writer
    splitMethodSource: definition
    into: [ :d :s | 
      declaration := d.
      source := s ].
  self assert: declaration equals: 'a: b b: c c: d'.
  self assert: source equals: ' ^ 42'
%

category: 'tests'
method: TonelWriterTest
testWriteClassDefinitionOn
  | writer def stream |
  writer := TonelWriter new
    packageWriter: self defaultPackageWriter;
    yourself.
  stream := TonelParser writeStreamClass on: String new.
  def := self
    creatClassDefinition: #'SomeObject'
    superclassName: #'Object'
    category: #'Kernel'
    instVarNames: #()
    classVarNames: #()
    poolDictionaryNames: #()
    classInstVarNames: #()
    type: #'normal'
    comment: 'comment test'.
  writer writeClassDefinition: def on: stream.
  self
    assert: stream contents
    equals:
      ('"
comment test
"
Class {
	#name : ''SomeObject'',
	#superclass : ''Object'',
	#category : ''Kernel''
}
'
        withLineEndings: TonelWriter lineEnding).
  stream := String new writeStreamPortable.
  def := self
    creatClassDefinition: #'SomeObject'
    superclassName: #'Object'
    category: #'Kernel'
    instVarNames: #(#'a' #'b' #'c')
    classVarNames: #(#'D' #'E')
    poolDictionaryNames: #(#'POOL')
    classInstVarNames: #(#'instVarA')
    type: #'normal'
    comment: 'comment test'.
  writer writeClassDefinition: def on: stream.
  self
    assert: stream contents
    equals:
      ('"
comment test
"
Class {
	#name : ''SomeObject'',
	#superclass : ''Object'',
	#instVars : [
		''a'',
		''b'',
		''c''
	],
	#classVars : [
		''D'',
		''E''
	],
	#pools : [
		''POOL''
	],
	#classInstVars : [
		''instVarA''
	],
	#category : ''Kernel''
}
'
        withLineEndings: TonelWriter lineEnding)
%

category: 'tests'
method: TonelWriterTest
testWriteMethodDefinitionOn
  | writer def stream |
  writer := TonelWriter new.
  stream := String new writeStreamPortable.
  def := self creatMethodDefinition: #'Object'
    classIsMeta: false
    selector: #'selector'
    category: 'accessing'
    source:
      'selector
	^ 42'.
  writer writeMethodDefinition: def on: stream.
  self
    assert: stream contents
    equals:
      ('
{ #category : ''accessing'' }
Object >> selector [
	^ 42
]
'
        withLineEndings: TonelWriter lineEnding).
  stream := String new writeStreamPortable.
  def := self creatMethodDefinition: #'Object'
    classIsMeta: true
    selector: #'selector'
    category: 'accessing'
    source:
      'selector
	^ 42'.
  writer writeMethodDefinition: def on: stream.
  self
    assert: stream contents
    equals:
      ('
{ #category : ''accessing'' }
Object class >> selector [
	^ 42
]
'
        withLineEndings: TonelWriter lineEnding).
  stream := String new writeStreamPortable.
  def := self creatMethodDefinition: #'Object'
    classIsMeta: false
    selector: #'='
    category: 'comparing'
    source:
      '= anObject
	^ self == anObject'.
  writer writeMethodDefinition: def on: stream.
  self
    assert: stream contents
    equals:
      ('
{ #category : ''comparing'' }
Object >> = anObject [
	^ self == anObject
]
'
        withLineEndings: TonelWriter lineEnding)
%

category: 'tests'
method: TonelWriterTest
testWritePackageOn
  | writer stream |
  writer := TonelWriter new.
  stream := TonelParser writeStreamClass on: String new.
  writer writePackage: 'ThePackage' on: stream.
  self
    assert: stream contents
    equals:
      ('Package { #name : ''ThePackage'' }
'
).

  stream := TonelParser writeStreamClass on: String new.
  writer writePackage: 'The-Package' on: stream.
  self
    assert: stream contents
    equals:
      ('Package { #name : ''The-Package'' }
'
).

  stream := TonelParser writeStreamClass on: String new.
  writer writePackage: 'The Package' on: stream.
  self
    assert: stream contents
    equals:
      ('Package { #name : ''The Package'' }
'
).
%

! Class implementation for 'TonelWriterForCypressTest'

!		Instance methods for 'TonelWriterForCypressTest'

category: 'private'
method: TonelWriterForCypressTest
creatClassDefinition: nameString superclassName: superclassString category: categoryString instVarNames: ivarArray classVarNames: cvarArray poolDictionaryNames: poolArray classInstVarNames: civarArray type: typeSymbol comment: commentString
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

category: 'private'
method: TonelWriterForCypressTest
creatClassDefinition: nameString superclassName: superclassString traitComposition: traitCompositionString classTraitComposition: classTraitCompositionString category: categoryString instVarNames: ivarArray classVarNames: cvarArray poolDictionaryNames: poolArray classInstVarNames: civarArray type: typeSymbol comment: commentString
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

category: 'private'
method: TonelWriterForCypressTest
creatMethodDefinition: classString classIsMeta: metaBoolean selector: selectorString category: catString source: sourceString
  ^ CypressMethodDefinition
    className: classString
    classIsMeta: metaBoolean
    selector: selectorString
    category: catString
    source: sourceString


%

category: 'private'
method: TonelWriterForCypressTest
defaultPackageWriter
  ^ TonelCypressWriter new
%

! Class extensions for 'CharacterCollection'

!		Instance methods for 'CharacterCollection'

category: '*tonel-gemstone-kernel'
method: CharacterCollection
endsWith: suffix

	"Answer whether the tail end of the receiver is the same as suffix.
	 The comparison is case-sensitive."

	| ofs |
	suffix size == 0
		ifTrue: [ ^ false ].
	(ofs := self size - suffix size) < 0
		ifTrue: [ ^ false ].
	^ self at: ofs + 1 equals: suffix	"
  'Elvis' endsWith: 'vis'
  'Elvis' endsWith: ''
"
%

category: '*tonel-gemstone-kernel'
method: CharacterCollection
findString: subString startingAt: startIndex caseSensitive: aBoolean

	^ self _findString: subString startingAt: startIndex ignoreCase: aBoolean not
%

category: '*tonel-gemstonecommon-core'
method: CharacterCollection
join: aCollection 
	"'*' join: #('WWWWW' 'W  EW' 'zzzz')
		->  'WWWWW*W  EW*zzzz' "
	^ self class new: (aCollection size * self size) streamContents: [:stream | 
			aCollection
				do: [:each | stream nextPutAll: each asString] 
				separatedBy: [stream nextPutAll: self]]
%

category: '*tonel-gemstonecommon-core'
method: CharacterCollection
lineIndicesDo: aBlock
	"execute aBlock with 3 arguments for each line:
	- start index of line
	- end index of line without line delimiter
	- end index of line including line delimiter(s) CR, LF or CRLF"
	
	| cr lf start sz nextLF nextCR |
	start := 1.
	sz := self size.
	cr := Character cr.
	nextCR := self indexOf: cr startingAt: 1.
	lf := Character lf.
	nextLF := self indexOf: lf startingAt: 1.
	[ start <= sz ] whileTrue: [
		(nextLF = 0 and: [ nextCR = 0 ])
			ifTrue: [ "No more CR, nor LF, the string is over"
					aBlock value: start value: sz value: sz.
					^self ].
		(nextCR = 0 or: [ 0 < nextLF and: [ nextLF < nextCR ] ])
			ifTrue: [ "Found a LF"
					aBlock value: start value: nextLF - 1 value: nextLF.
					start := 1 + nextLF.
					nextLF := self indexOf: lf startingAt: start ]
			ifFalse: [ 1 + nextCR = nextLF
				ifTrue: [ "Found a CR-LF pair"
					aBlock value: start value: nextCR - 1 value: nextLF.
					start := 1 + nextLF.
					nextCR := self indexOf: cr startingAt: start.
					nextLF := self indexOf: lf startingAt: start ]
				ifFalse: [ "Found a CR"
					aBlock value: start value: nextCR - 1 value: nextCR.
					start := 1 + nextCR.
					nextCR := self indexOf: cr startingAt: start ]]]
%

category: '*tonel-gemstone-kernel'
method: CharacterCollection
putOn: aStream

	^ aStream nextPutAll: self
%

category: '*tonel-gemstone-kernel'
method: CharacterCollection
substrings: separators 
	"Answer an array containing the substrings in the receiver separated 
	by the elements of separators."
	| result sourceStream subStringStream |
	
	(separators isString or: [ separators allSatisfy: [ :element | element isCharacter ] ])
		ifFalse: [ ^ self error: 'separators must be Characters.' ].
	sourceStream := self readStream.
	result := OrderedCollection new.
	subStringStream := String new writeStreamPortable.
	[ sourceStream atEnd ] whileFalse: [
		| char |
		char := sourceStream next.
		(separators includes: char)
			ifTrue: [
				subStringStream isEmpty ifFalse: [
					result add: subStringStream contents.
					subStringStream := String new writeStreamPortable ] ]
			ifFalse: [
				subStringStream nextPut: char ] ].
	subStringStream isEmpty ifFalse: [
		result add: subStringStream contents ].
	^ result asArray
%

category: '*tonel-gemstone-kernel'
method: CharacterCollection
trimBoth

	"Trim separators from both sides of the receiving string."

	^ self trimBoth: [ :char | char isSeparator ]
%

category: '*tonel-gemstone-kernel'
method: CharacterCollection
trimBoth: aBlock

	"Trim characters satisfying the condition given in aBlock from both sides of the receiving string."

	^ self trimLeft: aBlock right: aBlock
%

category: '*tonel-gemstone-kernel'
method: CharacterCollection
trimLeft

	"Trim separators from the left side of the receiving string."

	^ self trimLeft: [ :char | char isSeparator ]
%

category: '*tonel-gemstone-kernel'
method: CharacterCollection
trimLeft: aBlock

	"Trim characters satisfying the condition given in aBlock from the left side of the receiving string."

	^ self trimLeft: aBlock right: [ :char | false ]
%

category: '*tonel-gemstone-kernel'
method: CharacterCollection
trimLeft: aLeftBlock right: aRightBlock

	"Trim characters satisfying the condition given in aLeftBlock from the left side and aRightBlock from the right sides of the receiving string."

	| left right |
	left := 1.
	right := self size.
	[ left <= right and: [ aLeftBlock value: (self at: left) ] ]
		whileTrue: [ left := left + 1 ].
	[ left <= right and: [ aRightBlock value: (self at: right) ] ]
		whileTrue: [ right := right - 1 ].
	^ self copyFrom: left to: right
%

category: '*tonel-gemstone-kernel'
method: CharacterCollection
trimRight

	"Trim separators from the right side of the receiving string."

	^ self trimRight: [ :char | char isSeparator ]
%

category: '*tonel-gemstone-kernel'
method: CharacterCollection
trimRight: aBlock

	"Trim characters satisfying the condition given in aBlock from the right side of the receiving string."

	^ self trimLeft: [ :char | false ] right: aBlock
%

category: '*tonel-gemstonecommon-core'
method: CharacterCollection
withLineEndings: lineEndingString

	| stream |
	
	stream := nil.
	self lineIndicesDo: [ :start :endWithoutDelimiters :end |
		(stream isNil and: [ endWithoutDelimiters ~= end ]) ifTrue: [
			(self copyFrom: endWithoutDelimiters + 1 to: end) = lineEndingString ifFalse: [
				stream := WriteStreamPortable with: self copy.
				stream position: start - 1 ]].
		stream ifNotNil: [
			stream next: endWithoutDelimiters - start + 1 putAll: self startingAt: start.
			endWithoutDelimiters = end ifFalse: [
				stream nextPutAll: lineEndingString ]]].
	^stream
		ifNil: [ self ]
		ifNotNil: [ 
			stream position = self size
				ifTrue: [ stream originalContents ]
				ifFalse: [ stream contents ]]
%

! Class extensions for 'Collection'

!		Instance methods for 'Collection'

category: '*tonel-gemstonecommon-core'
method: Collection
asDictionary

  | dict |
  dict := Dictionary new.
  self do: [:assoc |
    dict add: assoc].
  ^ dict
%

category: '*tonel-gemstonecommon-core'
method: Collection
flattened
	
	"Flattens a collection of collections (no matter how many levels of collections exist).
	Strings are considered atoms and, as such, won't be flattened
	
	Examples:
	#(1 #(2 3) #(4 (#5))) flattened returns #(1 2 3 4 5) 
	#('string1' #('string2' 'string3')) flattened returns #('string1' 'string2' 'string3')"
	
	^ Array streamContents: [ :stream | self flattenOn: stream].
%

category: '*tonel-gemstonecommon-core'
method: Collection
flattenOn: aStream

	self do: [ :each | (each isCollection and: [each isString not]) 
						ifTrue: [each flattenOn: aStream]
						ifFalse: [aStream nextPut: each]].
%

category: '*tonel-gemstone-kernel'
method: Collection
ifNotEmpty: aBlock

	^ self size == 0
		ifFalse: [ aBlock cull: self ]
%

category: '*tonel-gemstone-kernel'
method: Collection
isCollection

	"Return true if the receiver is some sort of Collection and responds to basic collection messages such as #size and #do:"

	^ true
%

category: '*tonel-gemstone-kernel'
method: Collection
select: selectBlock thenDo: doBlock
  "Utility method to improve readability."

  ^ (self select: selectBlock) do: doBlock
%

category: '*tonel-gemstone-kernel'
method: Collection
sort: aSortBlock

	"Sort this array using aSortBlock. The block should take two arguments
	and return true if the first element should preceed the second one."

	^ self sortWithBlock: aSortBlock
%

! Class extensions for 'CypressClassDefinition'

!		Class methods for 'CypressClassDefinition'

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

!		Instance methods for 'CypressClassDefinition'

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

! Class extensions for 'CypressDefinition'

!		Instance methods for 'CypressDefinition'

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

! Class extensions for 'CypressGemStoneDirectoryUtilities'

!		Class methods for 'CypressGemStoneDirectoryUtilities'

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

! Class extensions for 'CypressMethodDefinition'

!		Instance methods for 'CypressMethodDefinition'

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

! Class extensions for 'CypressSnapshot'

!		Instance methods for 'CypressSnapshot'

category: '*cypresstonel-core'
method: CypressSnapshot
snapshot
  ^ self
%

! Class extensions for 'GsFile'

!		Instance methods for 'GsFile'

category: '*tonel-gemstonecommon-core'
method: GsFile
<< items

 	items putOn: self.
	
	^ self
%

! Class extensions for 'Object'

!		Instance methods for 'Object'

category: '*tonel-gemstone-kernel'
method: Object
isCollection

	"Return true if the receiver is some sort of Collection and responds to basic collection messages such as #size and #do:"

	^ false
%

category: '*tonel-gemstone-kernel'
method: Object
putOn: aStream

	^ aStream nextPut: self
%

! Class extensions for 'PositionableStreamPortable'

!		Instance methods for 'PositionableStreamPortable'

category: '*tonel-gemstonecommon-core'
method: PositionableStreamPortable
match: subCollection
  "Set the access position of the receiver to be past the next occurrence of the subCollection. Answer whether subCollection is found.  No wildcards, and case does matter."

  | pattern startMatch |
  pattern := ReadStreamPortable on: subCollection.
  startMatch := nil.
  [ pattern atEnd ]
    whileFalse: [ 
      self atEnd
        ifTrue: [ ^ false ].
      self next = pattern next
        ifTrue: [ 
          pattern position = 1
            ifTrue: [ startMatch := self position ] ]
        ifFalse: [ 
          pattern position: 0.
          startMatch
            ifNotNil: [ 
              self position: startMatch.
              startMatch := nil ] ] ].
  ^ true
%

category: '*tonel-gemstonecommon-core'
method: PositionableStreamPortable
originalContents
	"Answer the receiver's actual contents collection, NOT a copy.  1/29/96 sw"

	^ collection
%

! Class extensions for 'SequenceableCollection'

!		Instance methods for 'SequenceableCollection'

category: '*tonel-gemstone-kernel'
method: SequenceableCollection
allButLast

	"Answer a copy of the receiver containing all but the last
	element. Raise an error if there are not enough elements."

	^ self allButLast: 1
%

category: '*tonel-gemstone-kernel'
method: SequenceableCollection
allButLast: n

	"Answer a copy of the receiver containing all but the last n
	elements. Raise an error if there are not enough elements."

	^ self copyFrom: 1 to: self size - n
%

category: '*tonel-gemstone-kernel'
method: SequenceableCollection
beginsWith: aSequenceableCollection

	(aSequenceableCollection isEmpty
		or: [ self size < aSequenceableCollection size ])
		ifTrue: [ ^ false ].
	aSequenceableCollection
		withIndexDo: [ :each :index | 
			(self at: index) ~= each
				ifTrue: [ ^ false ] ].
	^ true
%

category: '*tonel-gemstone-kernel'
method: SequenceableCollection
fifth

	"Answer the fifth element of the receiver.
	Raise an error if there are not enough elements."

	^ self at: 5
%

category: '*tonel-gemstone-kernel'
method: SequenceableCollection
fourth

	"Answer the fourth element of the receiver.
	Raise an error if there are not enough elements."

	^ self at: 4
%

category: '*tonel-gemstonecommon-core'
method: SequenceableCollection
pairsCollect: aBlock 
	"Evaluate aBlock with my elements taken two at a time, and return an Array with the results"

	^ (1 to: self size // 2) collect:
		[:index | aBlock value: (self at: 2 * index - 1) value: (self at: 2 * index)]
"
#(1 'fred' 2 'charlie' 3 'elmer') pairsCollect:
	[:a :b | b, ' is number ', a printString]
"
%

category: '*tonel-gemstone-kernel'
method: SequenceableCollection
putOn: aStream

	self do: [ :each | each putOn: aStream ]
%

category: '*tonel-gemstone-kernel'
method: SequenceableCollection
second

	"Answer the second element of the receiver.
	Raise an error if there are not enough elements."

	^ self at: 2
%

category: '*tonel-gemstone-kernel'
method: SequenceableCollection
sixth

	"Answer the sixth element of the receiver.
	Raise an error if there are not enough elements."

	^ self at: 6
%

category: '*tonel-gemstone-kernel'
method: SequenceableCollection
sort

	"Sort this array into ascending order using the '<=' operator."

	^ self sort: [ :a :b | a <= b ]
%

category: '*tonel-gemstone-kernel'
method: SequenceableCollection
third

	"Answer the third element of the receiver.
	Raise an error if there are not enough elements."

	^ self at: 3
%

category: '*tonel-gemstone-kernel'
method: SequenceableCollection
withIndexDo: elementAndIndexBlock

	"Just like with:do: except that the iteration index supplies the second argument to the block."

	1 to: self size do: [ :index | elementAndIndexBlock value: (self at: index) value: index ]
%

category: '*tonel-gemstone-kernel'
method: SequenceableCollection
writeStreamPortable

	^ WriteStreamPortable on: self
%

! Class extensions for 'Stream'

!		Instance methods for 'Stream'

category: '*tonel-gemstone-kernel'
method: Stream
<< items

	items putOn: self
%

! Class extensions for 'Symbol'

!		Instance methods for 'Symbol'

category: '*tonel-gemstone-kernel'
method: Symbol
keywords

	"Answer an array of the keywords that compose the receiver."

	| kwd char keywords |
	keywords := Array new.
			kwd := WriteStreamPortable on: String new.
			1 to: self size do: [ :i | 
				kwd nextPut: (char := self at: i).
				char = $:
					ifTrue: [ 
						keywords add: kwd contents.
						kwd reset ] ].
			kwd position = 0
				ifFalse: [ keywords add: kwd contents ].
	(keywords size >= 1 and: [ (keywords at: 1) = ':' ])
		ifTrue: [ 
			"Has an initial keyword, as in #:if:then:else:"
			keywords := keywords allButFirst ].
	(keywords size >= 2 and: [ (keywords at: keywords size - 1) = ':' ])
		ifTrue: [ 
			"Has a final keyword, as in #nextPut::andCR"
			keywords := keywords
				copyReplaceFrom: keywords size - 1
				to: keywords size
				with: {(':' , keywords last)} ].
	^ keywords
%

! Class extensions for 'TonelAbstractWriterTest'

!		Instance methods for 'TonelAbstractWriterTest'

category: '*tonel-gemstone-tests'
method: TonelAbstractWriterTest
contentsOfFileNamed: fileName inDirectory: dir
  self fileUtils
    readStreamFor: fileName
    in: dir
    do: [ :stream | ^ stream contents ]
%

category: '*tonel-gemstone-tests'
method: TonelAbstractWriterTest
directory
  directory
    ifNil: [ 
      | fileUtils |
      fileUtils := self fileUtils.
      directory := fileUtils
        directoryFromPath: 'mctest'
        relativeTo: fileUtils default.
      fileUtils ensureDirectoryExists: directory ].
  ^ directory
%

category: '*tonel-gemstone-tests'
method: TonelAbstractWriterTest
directoryNamed: directoryName existsIn: dir
  | fileUtils filePath |
  fileUtils := self fileUtils.
  filePath := fileUtils directoryFromPath: directoryName relativeTo: dir.
  ^ fileUtils directoryExists: filePath
%

category: '*tonel-gemstone-tests'
method: TonelAbstractWriterTest
directoryNamed: directoryName in: dir
  | fileUtils |
  fileUtils := self fileUtils.
  ^ fileUtils directoryFromPath: directoryName relativeTo: dir
%

category: '*tonel-gemstone-tests'
method: TonelAbstractWriterTest
fileNamesIn: dir
  | fileUtils |
  fileUtils := self fileUtils.
  ^ fileUtils entryNamesFrom: dir
%

category: '*tonel-gemstone-tests'
method: TonelAbstractWriterTest
tearDown
  directory
    ifNotNil: [ :dir | 
      | fileUtils |
      fileUtils := self fileUtils.
      (fileUtils directoryExists: dir)
        ifTrue: [ fileUtils deleteAll: dir ] ].
  super tearDown
%

! Class extensions for 'TonelCypressReaderTest'

!		Instance methods for 'TonelCypressReaderTest'

category: '*cypresstonel-gemstone-tests'
method: TonelCypressReaderTest
createReaderFor: snapshot fileName: packageName
  | dir |
  dir := self newFileDirectorySnapshot: snapshot fileName: packageName.
  ^ TonelCypressReader on: dir fileName: packageName
%

category: '*cypresstonel-gemstone-tests'
method: TonelCypressReaderTest
directory
	directory ifNil:
		[ 
		| fileUtils |
		fileUtils := self fileUtils.
		directory := fileUtils
			directoryFromPath: 'mctest'
			relativeTo: fileUtils default.
		fileUtils ensureDirectoryExists: directory ].
	^ directory
%

category: '*cypresstonel-gemstone-tests'
method: TonelCypressReaderTest
newFileDirectorySnapshot: aSnapshot fileName: packageName
  "This method assumes writer works. If it is broken (the tests should show it), this tests 
	 will break too."

  | dir |
  dir := self directory.
  (TonelCypressWriter on: dir)
    writeSnapshot: aSnapshot
    inPackageNamed: packageName.
  ^ dir
%

category: '*cypresstonel-gemstone-tests'
method: TonelCypressReaderTest
tearDown
	self directory ifNotNil: [:dir | CypressFileUtilities current deleteAll: dir ].
	super tearDown.
%

! Class extensions for 'TonelParser'

!		Class methods for 'TonelParser'

category: '*tonel-gemstonecommon-core'
classmethod: TonelParser
readStreamClass

	^ ReadStreamPortable
%

category: '*tonel-gemstonecommon-core'
classmethod: TonelParser
writeStreamClass

	^ WriteStreamPortable
%

! Class extensions for 'TonelReaderTest'

!		Instance methods for 'TonelReaderTest'

category: '*tonel-gemstone-tests'
method: TonelReaderTest
directory
  directory
    ifNil: [ 
      | fileUtils |
      fileUtils := self fileUtils.
      directory := fileUtils
        directoryFromPath: 'mctest'
        relativeTo: fileUtils default.
      fileUtils ensureDirectoryExists: directory ].
  ^ directory
%

category: '*tonel-gemstone-tests'
method: TonelReaderTest
tearDown
  directory
    ifNotNil: [ :dir | 
      | fileUtils |
      fileUtils := self fileUtils.
      (fileUtils directoryExists: dir)
        ifTrue: [ fileUtils deleteAll: dir ] ].
  super tearDown
%

! Class extensions for 'TonelWriter'

!		Class methods for 'TonelWriter'

category: '*tonel-gemstonecommon-core'
classmethod: TonelWriter
lineEnding
  "Answer the os-specific line endings"

  ^ String with: Character lf
%

category: '*tonel-gemstonecommon-core'
classmethod: TonelWriter
orderedDictionaryClass
  "Answer the platform-specific OrderedDictionary-compatible class"

  ^ GsTonelOrderedDictionary
%

