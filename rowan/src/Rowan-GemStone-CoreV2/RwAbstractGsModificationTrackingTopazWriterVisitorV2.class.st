Class {
	#name : 'RwAbstractGsModificationTrackingTopazWriterVisitorV2',
	#superclass : 'RwAbstractGsModificationTopazWriterVisitorV2',
	#category : 'Rowan-GemStone-CoreV2'
}

{ #category : 'private exporting' }
RwAbstractGsModificationTrackingTopazWriterVisitorV2 >> _fileOutClassDeclaration: classDefinition on: aStream [
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

{ #category : 'private' }
RwAbstractGsModificationTrackingTopazWriterVisitorV2 >> _resetState [
	"clear the instance vars and get ready for the next package"

	currentPackageDefinition := currentClassDefinition := currentClassExtension := bufferedStream := classDefinitions := classExtensions := classInitializationDefinitions := classSymbolDictionaryNames := excludeClassInitializers := excludeRemoveAllMethods := nil
]

{ #category : 'private exporting' }
RwAbstractGsModificationTrackingTopazWriterVisitorV2 >> _stringForVariables: variableList [

	| stream |
	stream := WriteStreamPortable on: (String new: 100).
	variableList do: [:each | stream nextPutAll: each]
		separatedBy: [stream space].
	^stream contents
]

{ #category : 'private exporting' }
RwAbstractGsModificationTrackingTopazWriterVisitorV2 >> _writeClassTypeMessage: classDefinition on: aStream hasInstanceVariables: instanceVariableBlock [
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
RwAbstractGsModificationTrackingTopazWriterVisitorV2 >> exportClassDefinitions: classDefinitionsInOrder [
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
