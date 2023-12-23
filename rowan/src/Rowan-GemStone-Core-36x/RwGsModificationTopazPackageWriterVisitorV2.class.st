Class {
	#name : 'RwGsModificationTopazPackageWriterVisitorV2',
	#superclass : 'RwAbstractGsModificationTopazWriterVisitorV2',
	#instVars : [
		'packagesRoot'
	],
	#category : 'Rowan-GemStone-Core-36x'
}

{ #category : 'private exporting' }
RwGsModificationTopazPackageWriterVisitorV2 >> _fileOutClassDeclaration: classDefinition on: aStream [
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
						'	instVarNames: #(' , (self _stringForVariables: classDefinition instVarNames)
								, ')';
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
				'	classVars: #(' , (self _stringForVariables: classDefinition classVarNames)
						, ')';
		lf.
	hasClassInstVars
		ifTrue: [ 
			aStream
				nextPutAll:
						'	classInstVars: #('
								, (self _stringForVariables: classDefinition classInstVarNames) , ')';
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

{ #category : 'private' }
RwGsModificationTopazPackageWriterVisitorV2 >> _resetState [
	"clear the instance vars and get ready for the next package"

	currentPackageDefinition := currentClassDefinition := currentClassExtension := bufferedStream := classDefinitions := classExtensions := classInitializationDefinitions := classSymbolDictionaryNames := excludeClassInitializers := excludeRemoveAllMethods := nil
]

{ #category : 'private exporting' }
RwGsModificationTopazPackageWriterVisitorV2 >> _setBufferedStreamFor: filename [

	^ self _setBufferedStreamFor: filename extension: self filenameExtension
]

{ #category : 'private exporting' }
RwGsModificationTopazPackageWriterVisitorV2 >> _setBufferedStreamFor: filename extension: extension [

	| encodedStream |
	encodedStream := (self packagesRoot / filename, extension) writeStreamEncoded: 'utf8'.
	bufferedStream := ZnBufferedWriteStream on: encodedStream
]

{ #category : 'private exporting' }
RwGsModificationTopazPackageWriterVisitorV2 >> _stringForVariables: variableList [

	| stream |
	stream := WriteStreamPortable on: (String new: 100).
	variableList do: [:each | stream nextPutAll: each]
		separatedBy: [stream space].
	^stream contents
]

{ #category : 'private exporting' }
RwGsModificationTopazPackageWriterVisitorV2 >> _writeClassTypeMessage: classDefinition on: aStream hasInstanceVariables: instanceVariableBlock [
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

{ #category : 'exporting' }
RwGsModificationTopazPackageWriterVisitorV2 >> exportClassDefinitions: classDefinitionsInOrder [
	| stream |
	stream := self bufferedStream.
	classDefinitionsInOrder isEmpty not
		ifTrue: [ 
			stream
				nextPutAll: '! Class Declarations';
				lf ].
	classDefinitionsInOrder
		do: [ :classDef | self _fileOutClassDeclaration: classDef on: stream ]
]

{ #category : 'accessing' }
RwGsModificationTopazPackageWriterVisitorV2 >> packagesRoot [
	^packagesRoot
]

{ #category : 'accessing' }
RwGsModificationTopazPackageWriterVisitorV2 >> packagesRoot: object [
	packagesRoot := object
]

{ #category : 'class writing' }
RwGsModificationTopazPackageWriterVisitorV2 >> processClass: aClassModification [
	| classDefinition symbolDictName clsName |
	classDefinition := aClassModification after.
	(self classDefinitions at: (clsName := classDefinition name) ifAbsent: [  ])
		ifNotNil: [ 
			self
				error:
					'duplicate class definition for ' , clsName printString , ' encountered.' ].

	symbolDictName := self currentProjectDefinition
		gemstoneSymbolDictNameForPackageNamed: self currentPackageDefinition name.
	self classSymbolDictionaryNames at: classDefinition name put: symbolDictName.
	self classDefinitions at: classDefinition name put: classDefinition	"no need to visit any further as the class definition records the instance and class methods"
]

{ #category : 'class writing' }
RwGsModificationTopazPackageWriterVisitorV2 >> processClassExtension: aClassExtensionModification [

	| classExtension |
	classExtension := aClassExtensionModification after.
	(self classExtensions at: classExtension name ifAbsentPut: [ Set new ])
		add: classExtension.

	"no need to visit any further as the class etension records the instance and class methods"
]

{ #category : 'package writing' }
RwGsModificationTopazPackageWriterVisitorV2 >> processPackage: aPackageModification [
	| packageName classDefinitionsList classDefinitionsInOrder classExtensionsList classExtensionsInOrder |
	packageName := aPackageModification after name.
	super processPackage: aPackageModification.
	self _setBufferedStreamFor: packageName.
	self bufferedStream
		nextPutAll: self topazFileHeader;
		lf;
		nextPutAll: 'SET PACKAGE: ' , packageName;
		lf.

	classInitializationDefinitions := Set new.	"per file record"
	classDefinitionsList := self classDefinitions values.

	classDefinitionsInOrder := (RowanGsGeneralDependencySorter
		on: classDefinitionsList
		dependsOn: [ :candidate | candidate superclassName ]
		dependent: [ :candidate | candidate name ]) inOrder.
	self
		exportClassDefinitions: classDefinitionsInOrder;
		exportMethodDefinitions: classDefinitionsInOrder.	"consolidate the classExtensions for a class from multiple packages into a single definition"
	classExtensionsList := Set new.
	self classExtensions
		keysAndValuesDo: [ :classExtName :extsInConfig | 
			extsInConfig size <= 1
				ifTrue: [ classExtensionsList addAll: extsInConfig ]
				ifFalse: [ 
					| ar def |
					ar := extsInConfig asArray.
					def := (ar at: 1) copy.
					2 to: ar size do: [ :index | 
						| d |
						d := ar at: index.
						d classMethodDefinitions values
							do: [ :m | def addClassMethodDefinition: m ].
						d instanceMethodDefinitions values
							do: [ :m | def addInstanceMethodDefinition: m ] ].
					classExtensionsList add: def ] ].

	classExtensionsInOrder := classExtensionsList
		sort: [ :a :b | a name <= b name ].
	self exportExtensionMethodDefinitions: classExtensionsInOrder.

	self exportClassInitializations.

	self bufferedStream nextPutAll: self topazFileFooter.

	self bufferedStream
		flush;
		close.
	self _resetState
]
