"
No class-specific documentation for RwGsModificationTopazWriterVisitorV2, hierarchy is:
Object
  RwAbstractReaderWriterVisitor( currentProjectDefinition packageConvention currentPackageDefinition currentClassDefinition currentClassExtension)
    RwModificationWriterVisitor
      RwGsModificationTopazWriterVisitorV2( topazFilenameComponentMap topazFilename topazFileHeader filenameExtension classSymbolDictionaryNames classDefinitions classExtensions bufferedStream topazFilenamePackageNamesMap classDefPackageNameMap classExtPackageNameMap classInitializationDefinitions buildPackageNamesMap repositoryRootPath)

"
Class {
	#name : 'RwGsModificationTopazWriterVisitorV2',
	#superclass : 'RwAbstractGsModificationTopazWriterVisitorV2',
	#instVars : [
		'topazFilenameComponentMap',
		'topazFilename',
		'fileNamesInFileInOrder',
		'topazFilenamePackageNamesMap',
		'classDefPackageNameMap',
		'classExtPackageNameMap',
		'buildPackageNamesMap',
		'repositoryRootPath'
	],
	#category : 'Rowan-GemStone-CoreV2'
}

{ #category : 'private exporting' }
RwGsModificationTopazWriterVisitorV2 >> _fileOutClassDeclaration: classDefinition on: aStream [
	| optionsString reservedOopString hasClassInstVars |
	aStream
		nextPutAll: 'doit';
		lf;
		nextPutAll: '(' , classDefinition superclassName;
		lf.
	hasClassInstVars := self
		_writeClassTypeMessage: classDefinition
		on: aStream
		hasInstanceVariables: [ 
			aStream
				nextPutAll:
						'	instVarNames: #( ' , (self _stringForVariables: classDefinition instVarNames)
								, ' )';
				lf ].
	optionsString := String new.
	classDefinition gs_options isEmpty
		ifFalse: [ 
			optionsString := ' ' , (self _symbolsForVariables: classDefinition gs_options)
				, ' ' ].
	self logCreation
		ifTrue: [ 
			"for verbose logging during filein and upgrade"
			optionsString addAll: ' #logCreation ' ].
	reservedOopString := ''.
	classDefinition gs_reservedOop isEmpty
		ifFalse: [ 
			| strm |
			strm := WriteStream on: String new.
			strm
				nextPutAll: '	reservedOop: ' , classDefinition gs_reservedOop;
				lf.
			reservedOopString := strm contents ].
	aStream
		nextPutAll:
				'	classVars: #( ' , (self _stringForVariables: classDefinition classVarNames)
						, ' )';
		lf.
	hasClassInstVars
		ifTrue: [ 
			aStream
				nextPutAll:
						'	classInstVars: #( '
								, (self _stringForVariables: classDefinition classInstVarNames) , ' )';
				lf ].
	aStream
		nextPutAll: '	poolDictionaries: #()';
		lf;
		nextPutAll:
				'	inDictionary: ' , (self classSymbolDictionaryNames at: classDefinition name);
		lf;
		nextPutAll: '	options: #(' , optionsString , ')';
		lf;
		nextPutAll: reservedOopString;
		nextPutAll: ')';
		lf;
		nextPutAll: '		category: ' , classDefinition category printString , ';';
		lf;
		yourself.
	classDefinition comment isEmpty
		ifFalse: [ 
			aStream
				nextPutAll: '		comment: ' , classDefinition comment printString , ';';
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
RwGsModificationTopazWriterVisitorV2 >> _setBufferedStreamFor: filename [

	^ self _setBufferedStreamFor: filename extension: self filenameExtension
]

{ #category : 'private exporting' }
RwGsModificationTopazWriterVisitorV2 >> _stringForVariables: variableList [

	| stream |
	stream := WriteStreamPortable on: (String new: 100).
	variableList do: [:each | stream nextPutAll: each]
		separatedBy: [stream space].
	^stream contents
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
		nextPutAll: classTypeMessage , classDefinition name asString printString;
		lf.
	hasInstanceVariables
		ifTrue: [ instanceVariableBlock value ].
	^ hasClassInstVars
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitorV2 >> buildPackageNamesMap [

	"If true, topazFilenamePackageNamesMap will be built from topazFilenameConfigsMap.
		If false, existing topazFilenamePackageNamesMap will be used"

	^ buildPackageNamesMap ifNil: [ buildPackageNamesMap := true ]
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitorV2 >> classDefPackageNameMap [

	^ classDefPackageNameMap ifNil: [ classDefPackageNameMap := Dictionary new ]
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitorV2 >> classExtPackageNameMap [

	^ classExtPackageNameMap ifNil: [ classExtPackageNameMap := IdentityKeyValueDictionary new ]
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

{ #category : 'class writing' }
RwGsModificationTopazWriterVisitorV2 >> processClass: aClassModification [

	| classDefinition symbolDictName clsName |
	classDefinition := aClassModification after.
	(self classDefinitions at: (clsName := classDefinition name) ifAbsent: []) ifNotNil: [ 
   self error: 'duplicate class definition for ', clsName printString, ' encountered.'].

	symbolDictName := self currentProjectDefinition symbolDictNameForPackageNamed: self currentPackageDefinition name.
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
			| readTool |
			topazFilenamePackageNamesMap := self topazFilenamePackageNamesMap.
			readTool := Rowan projectTools readV2.
			self topazFilenameComponentMap
				keysAndValuesDo: [ :filename :componentAndPlatformConditionalAttributesMap | 
					| componentNames platformConditionalAttributes visitor packageNames componentAndPlatformConditionalAttributes |
					componentAndPlatformConditionalAttributes := componentAndPlatformConditionalAttributesMap
						at: currentProjectDefinition name
						ifAbsent: [ 
							{{}.
							{}} ].
					componentAndPlatformConditionalAttributes
						ifNotNil: [ 
							componentNames := componentAndPlatformConditionalAttributes at: 1.
							platformConditionalAttributes := componentAndPlatformConditionalAttributes
								at: 2.
							componentNames isEmpty
								ifFalse: [ 
									currentProjectDefinition componentsRoot exists
										ifTrue: [ 
											"read the project from disk, if it is present on disk"
											visitor := readTool
												readProjectForResolvedProject: currentProjectDefinition
												withComponentNames: componentNames
												platformConditionalAttributes: platformConditionalAttributes ] ].
							packageNames := visitor
								ifNil: [ 
									self topazFilenameComponentMap size > 1
										ifTrue: [ 
											self
												error:
													'unable to read project components for project '
														, currentProjectDefinition name printString
														, '. Multiple output files likely to have the same contents.' ].
									currentProjectDefinition packageNames ]
								ifNotNil: [ visitor packageNames ].
							(topazFilenamePackageNamesMap at: filename ifAbsentPut: [ Set new ])
								addAll: packageNames ] ] ].
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
RwGsModificationTopazWriterVisitorV2 >> topazFilename [

	^ topazFilename
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitorV2 >> topazFilename: aString [

	topazFilename := aString
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitorV2 >> topazFilenameComponentMap [

	^ topazFilenameComponentMap
		ifNil: [ topazFilenameComponentMap := Dictionary new at: self topazFilename put: Dictionary new; yourself ]
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitorV2 >> topazFilenameComponentMap: aDictionary [

	"keys are topaz file names, values are a dictionary:
		whose keys are project names and values are two slot array with component name list and customConditionalAttributes. 
			The component names and customConditionalAttributes are resolved to a list of packages that will be written to the named file.

			If the component/customConditionalAttributes list is empty, the default component and customConditionalAttributes for the project will be used.
			If either (or both) of the slots contain empty array, then the corresponding project default will be used."

	"If topazFilenamePackageNamesMap is explicitly set, then the contents of topazFilenameComponentMap will be ignored."

	buildPackageNamesMap := true.
	topazFilenameComponentMap := aDictionary
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitorV2 >> topazFilenamePackageNamesMap [

	^ topazFilenamePackageNamesMap ifNil: [ topazFilenamePackageNamesMap := Dictionary new ]
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitorV2 >> topazFilenamePackageNamesMap: aDictionary [

	"keys are topaz file names, values are a collection of package names"

	"if topazFilenameConfigsMap is being used, then the topazFilenamePackageNamesMap is generated automatically,
		based on the project configurations."

	"If you explicitly set topazFilenamePackageNamesMap then contents of topazFilenameConfigsMap will be ignored."

	buildPackageNamesMap := false.
	topazFilenamePackageNamesMap := aDictionary
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitorV2 >> topazFileReference [

	^ self repositoryRootPath / self topazFilename, self filenameExtension
]
