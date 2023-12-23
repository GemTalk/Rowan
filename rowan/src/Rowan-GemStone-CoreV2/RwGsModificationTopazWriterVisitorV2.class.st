"
No class-specific documentation for RwGsModificationTopazWriterVisitorV2, hierarchy is:
Object
  RwAbstractReaderWriterVisitor( currentProjectDefinition packageConvention currentPackageDefinition currentClassDefinition currentClassExtension)
    RwModificationWriterVisitor
      RwGsModificationTopazWriterVisitorV2( topazFilenameComponentMap topazFilename topazFileHeader filenameExtension classSymbolDictionaryNames classDefinitions classExtensions bufferedStream topazFilenamePackageNamesMap classDefPackageNameMap classExtPackageNameMap classInitializationDefinitions buildPackageNamesMap repositoryRootPath)

"
Class {
	#name : 'RwGsModificationTopazWriterVisitorV2',
	#superclass : 'RwModificationWriterVisitor',
	#instVars : [
		'topazFilename',
		'topazFileHeader',
		'topazFileFooter',
		'excludeClassInitializers',
		'excludeRemoveAllMethods',
		'fileNamesInFileInOrder',
		'logCreation',
		'filenameExtension',
		'classSymbolDictionaryNames',
		'classDefinitions',
		'classExtensions',
		'bufferedStream',
		'topazFilenamePackageNamesMap',
		'classDefPackageNameMap',
		'classExtPackageNameMap',
		'classInitializationDefinitions',
		'buildPackageNamesMap',
		'repositoryRootPath'
	],
	#classVars : [
		'Character_lf'
	],
	#category : 'Rowan-GemStone-CoreV2'
}

{ #category : 'initialization' }
RwGsModificationTopazWriterVisitorV2 class >> initialize [
	self _addInvariantClassVar: #Character_lf value: Character lf
]

{ #category : 'private exporting' }
RwGsModificationTopazWriterVisitorV2 >> _fileOutClassDeclaration: classDefinition on: aStream [
	| optionsString reservedOopString hasClassInstVars |
	aStream
		nextPutAll: 'doit';
		lf;
		nextPutAll: '(' ; nextPutAll: classDefinition superclassName;
		lf.
	hasClassInstVars := self
		_writeClassTypeMessage: classDefinition
		on: aStream
		hasInstanceVariables: [ 
			aStream
				nextPutAll: '	instVarNames: #(' .
      self _stringForVariables: classDefinition instVarNames to: aStream .
      aStream nextPutAll: ')' ; lf ].
	optionsString := String new.
	classDefinition gs_options isEmpty
		ifFalse: [ 
			optionsString := String new .
      self _symbolsForVariables: classDefinition gs_options toString: optionsString .
		  ].
	self logCreation
		ifTrue: [ 
			"for verbose logging during filein and upgrade"
			optionsString addAll: ' #logCreation ' ].
	reservedOopString := String new .
	classDefinition gs_reservedOop isEmpty
		ifFalse: [ 
			reservedOopString addAll: '	reservedOop: ' ; addAll: classDefinition gs_reservedOop;
				lf.
		].
	aStream
		nextPutAll: '	classVars: #(' . 
  self _stringForVariables: classDefinition classVarNames to: aStream .
  aStream nextPutAll:')' ; lf .
	hasClassInstVars
		ifTrue: [ 
			aStream nextPutAll: '	classInstVars: #(' .
			self _stringForVariables: classDefinition classInstVarNames to: aStream .
      aStream nextPutAll:')' ; lf 
    ].
	aStream
		nextPutAll: '	poolDictionaries: #()';
		lf;
		nextPutAll: '	inDictionary: ' ; nextPutAll: (self classSymbolDictionaryNames at: classDefinition name);
		lf;
		nextPutAll: '	options: #(' ; nextPutAll: optionsString ; nextPut: $) ;
		lf;
		nextPutAll: reservedOopString;
		nextPutAll: ')';
		lf;
		nextPutAll: '		category: '; nextPutAll: classDefinition category printString ; nextPut: $; ;
		lf .
	classDefinition comment size == 0
		ifFalse: [ 
			aStream
				nextPutAll: '		comment: '; nextPutAll: classDefinition comment printString; nextPut: $; ;
				lf;
				yourself ].
	aStream
		nextPutAll: '		immediateInvariant.';
		lf;
		nextPutAll: 'true.';
		lf;
		nextPutAll: '%';
		lf;
		lf.
	self _fileoutRemoveAllMethodsFor: classDefinition name on: aStream
]

{ #category : 'private exporting' }
RwGsModificationTopazWriterVisitorV2 >> _fileOutMethod: methodDefinition forClass: className isMeta: isMeta on: aStream [

	aStream
		nextPutAll: 'category: '; nextPutAll: methodDefinition protocol printString; lf;
		nextPutAll: (isMeta ifTrue: ['classmethod: '] ifFalse: ['method: ']) ; nextPutAll: className; lf;
		nextPutAll: methodDefinition source.
	methodDefinition source last == Character_lf
		ifFalse: [aStream lf].
	aStream nextPut: $% ; lf;
		lf
]

{ #category : 'private exporting' }
RwGsModificationTopazWriterVisitorV2 >> _fileoutRemoveAllMethodsFor: className on: aStream [
	self excludeRemoveAllMethods
		ifFalse: [ 
			aStream
				nextPutAll: 'removeallmethods ' , className;
				lf;
				nextPutAll: 'removeallclassmethods ' , className;
				lf;
				lf ]
]

{ #category : 'private exporting' }
RwGsModificationTopazWriterVisitorV2 >> _setBufferedStreamFor: filename [

	^ self _setBufferedStreamFor: filename extension: self filenameExtension
]

{ #category : 'private exporting' }
RwGsModificationTopazWriterVisitorV2 >> _setBufferedStreamFor: filename extension: extension [

	| encodedStream |
	encodedStream := (self repositoryRootPath / filename, extension) writeStreamEncoded: 'utf8'.
	bufferedStream := ZnBufferedWriteStream on: encodedStream
]

{ #category : 'private exporting' }
RwGsModificationTopazWriterVisitorV2 >> _stringForVariables: variableList to: aStream [
	| leadingSpace |
	leadingSpace := false.
	variableList
		do: [ :each | 
			leadingSpace
				ifTrue: [ aStream nextPut: $  ]
				ifFalse: [ leadingSpace := true ].
			aStream nextPutAll: each ]
]

{ #category : 'private exporting' }
RwGsModificationTopazWriterVisitorV2 >> _symbolsForVariables: variableList toString: aString [
	| trailingSpace |
	trailingSpace := false.
	variableList
		do: [ :each | 
			trailingSpace := true.
			aString
				add: $ ;
				addAll: each asSymbol printString ].
	trailingSpace
		ifTrue: [ aString add: $  ]
]

{ #category : 'private exporting' }
RwGsModificationTopazWriterVisitorV2 >> _writeClassTypeMessage: classDefinition on: aStream hasInstanceVariables: instanceVariableBlock [
	| classType classTypeMessage hasInstanceVariables hasReservedOop hasClassInstVars |
	hasInstanceVariables := true.
	hasReservedOop := classDefinition gs_reservedOop isEmpty not.
	hasClassInstVars := true.
	classType := classDefinition subclassType.
	classType = 'variable'
		ifTrue: [ 
			classTypeMessage := hasReservedOop
				ifTrue: [ '_newKernelIndexableSubclass:' ]
				ifFalse: [ 'indexableSubclass: ' ] ]
		ifFalse: [ 
			classType = 'byteSubclass'
				ifTrue: [ 
					classTypeMessage := hasReservedOop
						ifTrue: [ '_newKernelByteSubclass:' ]
						ifFalse: [ 'byteSubclass: ' ].
					hasClassInstVars := hasInstanceVariables := false ]
				ifFalse: [ 
					(classType = '' or: [ classType = 'immediate' ])
						ifTrue: [ 
							classTypeMessage := hasReservedOop
								ifTrue: [ '_newKernelSubclass:' ]
								ifFalse: [ 'subclass: ' ] ]
						ifFalse: [ self error: 'unknown subclass type: ' , classType ] ] ].
	aStream
		tab;
		nextPutAll: classTypeMessage ; nextPutAll: classDefinition name asString printString;
		lf.
	hasInstanceVariables
		ifTrue: [ instanceVariableBlock value ].
	^ hasClassInstVars
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitorV2 >> bufferedStream [

	^ bufferedStream ifNil: [ 
		| encodedStream |
		encodedStream := (self repositoryRootPath / self topazFilename, self filenameExtension) writeStreamEncoded: 'utf8'.
		bufferedStream := ZnBufferedWriteStream on: encodedStream ]
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitorV2 >> buildPackageNamesMap [
	"If true, topazFilenamePackageNamesMap will be created by using the package names for each project.
		If false, existing topazFilenamePackageNamesMap will be used"

	^ buildPackageNamesMap ifNil: [ buildPackageNamesMap := true ]
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitorV2 >> classDefinitions [

	^ classDefinitions ifNil: [ classDefinitions := Dictionary new ]
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitorV2 >> classDefPackageNameMap [

	^ classDefPackageNameMap ifNil: [ classDefPackageNameMap := Dictionary new ]
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitorV2 >> classExtensions [

	^ classExtensions ifNil: [ classExtensions := Dictionary new ]
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitorV2 >> classExtPackageNameMap [

	^ classExtPackageNameMap ifNil: [ classExtPackageNameMap := IdentityKeyValueDictionary new ]
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitorV2 >> classInitializationDefinitions [

	^ classInitializationDefinitions
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitorV2 >> classSymbolDictionaryNames [

	^ classSymbolDictionaryNames ifNil: [ classSymbolDictionaryNames := Dictionary new ]
]

{ #category : 'actions' }
RwGsModificationTopazWriterVisitorV2 >> deletededClass: aClassModification [

	"a deleted class is ignored for topaz ... the deleted class is simply not written"
]

{ #category : 'actions' }
RwGsModificationTopazWriterVisitorV2 >> deletededClassExtension: aClassExtensionModification [

	"a deleted class extension is ignored for topaz ... the deleted methods are simply not written"
]

{ #category : 'actions' }
RwGsModificationTopazWriterVisitorV2 >> deletededPackage: aPackageModification [

	"a deleted package is ignored for topaz ... the deleted classes and methods are simply not written out"
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitorV2 >> excludeClassInitializers [
	^ excludeClassInitializers ifNil: [ false ]
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitorV2 >> excludeClassInitializers: aBool [
	excludeClassInitializers := aBool
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitorV2 >> excludeRemoveAllMethods [
	^ excludeRemoveAllMethods ifNil: [ false ]
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitorV2 >> excludeRemoveAllMethods: aBool [
	excludeRemoveAllMethods := aBool
]

{ #category : 'exporting' }
RwGsModificationTopazWriterVisitorV2 >> export [

	self fileNamesInFileInOrder do: [:filename | 
		| packageNames classDefinitionsInOrder classExtensionsInOrder classExtensionsList classDefinitionsList |
		packageNames := self topazFilenamePackageNamesMap at: filename.
		self _setBufferedStreamFor: filename.
		self bufferedStream nextPutAll: self topazFileHeader.

		classInitializationDefinitions := Set new. "per file record"
		classDefinitionsList := Set new.

		self classDefPackageNameMap keysAndValuesDo: [:className :packageName |
			(packageNames includes: packageName)
				ifTrue: [ classDefinitionsList add: (self classDefinitions at: className) ] ].

		classDefinitionsInOrder := (RowanGsGeneralDependencySorter
			on: classDefinitionsList
			dependsOn: [:candidate | candidate superclassName]
			dependent: [:candidate | candidate name]) inOrder.
		self exportClassDefinitions: classDefinitionsInOrder;
			exportMethodDefinitions: classDefinitionsInOrder.

		"consolidate the classExtensions for a class from multiple packages into a single definition"
		classExtensionsList := Set new. 
		self classExtensions keysAndValuesDo: [:classExtName :extSet |
			| extsInConfig |
			extsInConfig := Set new.
			extSet do: [:ext |
				| packageName |
				packageName := self classExtPackageNameMap at: ext.
				(packageNames includes: packageName)
					ifTrue: [ extsInConfig add: ext ] ].

			extsInConfig size <= 1 
				ifTrue: [ classExtensionsList addAll: extsInConfig ]
				ifFalse: [
					| ar def |
					ar := extsInConfig asArray.
					def := (ar at: 1) copy.
					2 to: ar size do: [:index |
						| d |
						d := (ar at: index).
						d classMethodDefinitions values do: [:m | def addClassMethodDefinition: m ].
						d instanceMethodDefinitions values do: [:m | def addInstanceMethodDefinition: m ] ].
					classExtensionsList add: def ] ].

		classExtensionsInOrder := classExtensionsList sort: [:a :b | a name <= b name ].
		self
			exportExtensionMethodDefinitions: classExtensionsInOrder.

		self exportClassInitializations.

		self bufferedStream nextPutAll: self topazFileFooter.

		self bufferedStream flush; close ].
]

{ #category : 'exporting' }
RwGsModificationTopazWriterVisitorV2 >> exportClassDefinitions: classDefinitionsInOrder [

	| stream |
	stream := self bufferedStream.
	classDefinitionsInOrder isEmpty not
		ifTrue: [ 
			stream
				nextPutAll: '! Class Declarations'; lf;
				nextPutAll: '! Generated file, do not Edit'; lf;
				lf ].
	classDefinitionsInOrder do: [:classDef | 
		self _fileOutClassDeclaration: classDef on: stream ].
]

{ #category : 'exporting' }
RwGsModificationTopazWriterVisitorV2 >> exportClassInitializations [
	| stream exclude |
	self classInitializationDefinitions isEmpty ifTrue: [ ^ self ].
	stream := self bufferedStream.
  exclude := self excludeClassInitializers.
  exclude ifTrue:[
    stream nextPutAll: '! Class Initialization Excluded by export visitor'; lf .
  ] ifFalse:[
	  stream 
		  nextPutAll: '! Class Initialization'; lf;
      lf;
		  nextPutAll: 'run'; lf.
  ].
	(self classInitializationDefinitions sort: [:a :b | a name <= b name ]) do: [ :classDef |
    exclude ifTrue:[ stream nextPutAll:'!  ' ].
		stream nextPutAll: classDef name ; nextPutAll: ' initialize.'; lf 
  ].
  exclude ifFalse:[
	  stream nextPutAll: 'true'; lf;
	  nextPutAll: '%'; lf .
  ].
]

{ #category : 'exporting' }
RwGsModificationTopazWriterVisitorV2 >> exportExtensionMethodDefinitions: classDefinitionsInOrder [

	self exportMethodDefinitions: classDefinitionsInOrder labeled: 'Class extensions'
]

{ #category : 'exporting' }
RwGsModificationTopazWriterVisitorV2 >> exportMethodDefinitions: classDefinitionsInOrder [

	self exportMethodDefinitions: classDefinitionsInOrder labeled: 'Class implementation'
]

{ #category : 'exporting' }
RwGsModificationTopazWriterVisitorV2 >> exportMethodDefinitions: classDefinitionsInOrder labeled: label [
	| stream |
	stream := self bufferedStream.
	classDefinitionsInOrder do: [:classDef | 
		| className |
		className := classDef name.
		(classDef classMethodDefinitions isEmpty not or: [ classDef instanceMethodDefinitions isEmpty not])
			ifTrue: [ 
				stream nextPutAll: '! '; nextPutAll: label ; nextPutAll: ' for ' ; nextPutAll: className printString; lf;
				lf ].
		classDef classMethodDefinitions isEmpty not
			ifTrue: [ 
				stream nextPutAll: '!		Class methods for ' ; nextPutAll: className printString; lf;
				lf ].
		(classDef classMethodDefinitions values sort: [:a :b | a selector <= b selector ])
			do: [:methodDef |
				methodDef selector == #initialize
					ifTrue: [ self classInitializationDefinitions add: classDef ].
				self _fileOutMethod: methodDef forClass: className isMeta: true on: stream ].
		classDef instanceMethodDefinitions isEmpty 
			ifFalse: [ 
				stream nextPutAll: '!		Instance methods for ' ; nextPutAll: className printString; lf;
				lf ].
		(classDef instanceMethodDefinitions values sort: [:a :b | a selector <= b selector ])
			do: [:methodDef |
				self _fileOutMethod: methodDef forClass: className isMeta: false on: stream ] ].
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitorV2 >> filenameExtension [

	^ filenameExtension ifNil: ['gs' ]
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitorV2 >> filenameExtension: aString [

	filenameExtension := aString
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitorV2 >> fileNamesInFileInOrder [
	"Normally the order that fileins are created in does not matter"

	^ fileNamesInFileInOrder ifNil: [ self topazFilenamePackageNamesMap keys ]
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitorV2 >> fileNamesInFileInOrder: object [
	fileNamesInFileInOrder := object
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitorV2 >> logCreation [
	^logCreation ifNil: [ ^false ]
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitorV2 >> logCreation: object [
	logCreation := object
]

{ #category : 'class writing' }
RwGsModificationTopazWriterVisitorV2 >> processClass: aClassModification [

	| classDefinition symbolDictName clsName |
	classDefinition := aClassModification after.
	(self classDefinitions at: (clsName := classDefinition name) ifAbsent: []) ifNotNil: [ 
   self error: 'duplicate class definition for ', clsName printString, ' encountered.'].

	symbolDictName := self currentProjectDefinition gemstoneSymbolDictNameForPackageNamed: self currentPackageDefinition name.
	self classSymbolDictionaryNames at: classDefinition name put: symbolDictName.
	self classDefinitions at: classDefinition name put: classDefinition.

	self classDefPackageNameMap at: classDefinition name put: currentPackageDefinition name.

	"no need to visit any further as the class definition records the instance and class methods"
]

{ #category : 'class writing' }
RwGsModificationTopazWriterVisitorV2 >> processClassExtension: aClassExtensionModification [

	| classExtension |
	classExtension := aClassExtensionModification after.
	(self classExtensions at: classExtension name ifAbsentPut: [ Set new ])
		add: classExtension.

	self classExtPackageNameMap at: classExtension put: currentPackageDefinition name.

	"no need to visit any further as the class etension records the instance and class methods"
]

{ #category : 'project writing' }
RwGsModificationTopazWriterVisitorV2 >> processProject: aProjectModification [
	self buildPackageNamesMap
		ifTrue: [ 
			"topazFilenamePackageNamesMap not explicitly set, accumulate the current project package names in topaz file name"
			topazFilenamePackageNamesMap := self topazFilenamePackageNamesMap.
			(topazFilenamePackageNamesMap at: self topazFilename ifAbsentPut: [ Set new ])
				addAll: currentProjectDefinition packageNames ].
	aProjectModification packagesModification acceptVisitor: self
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitorV2 >> repositoryRootPath [

	^ repositoryRootPath
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitorV2 >> repositoryRootPath: aStringOrFileReference [ 

	repositoryRootPath := aStringOrFileReference asFileReference.
	repositoryRootPath exists ifFalse: [ self error: 'The repository root path must exist: ', repositoryRootPath pathString printString ]
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitorV2 >> topazFileFooter [
	^ topazFileFooter ifNil: [ '' ]
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitorV2 >> topazFileFooter: object [
	topazFileFooter := object
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitorV2 >> topazFileHeader [

	^ topazFileHeader ifNil: [ '' ]
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitorV2 >> topazFileHeader: aString [

	topazFileHeader := aString
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitorV2 >> topazFilename [

	^ topazFilename
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitorV2 >> topazFilename: aString [

	topazFilename := aString
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitorV2 >> topazFilenamePackageNamesMap [
	^ topazFilenamePackageNamesMap
		ifNil: [ topazFilenamePackageNamesMap := Dictionary new ]
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitorV2 >> topazFilenamePackageNamesMap: aDictionary [
	"keys are topaz file names, values are a collection of package names"

	buildPackageNamesMap := false.	"prevent automatic generation of map (see processProject:)"
	topazFilenamePackageNamesMap := aDictionary
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitorV2 >> topazFileReference [

	^ self repositoryRootPath / self topazFilename, self filenameExtension
]
