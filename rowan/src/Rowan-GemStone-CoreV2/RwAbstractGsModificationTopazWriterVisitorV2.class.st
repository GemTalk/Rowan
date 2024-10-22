Class {
	#name : 'RwAbstractGsModificationTopazWriterVisitorV2',
	#superclass : 'RwModificationWriterVisitor',
	#instVars : [
		'bufferedStream',
		'classDefinitions',
		'classExtensions',
		'classInitializationDefinitions',
		'classSymbolDictionaryNames',
		'excludeClassInitializers',
		'excludeRemoveAllMethods',
		'filenameExtension',
		'logCreation',
		'topazFileFooter',
		'topazFileHeader'
	],
	#category : 'Rowan-GemStone-CoreV2'
}

{ #category : 'private exporting' }
RwAbstractGsModificationTopazWriterVisitorV2 >> _fileOutMethod: methodDefinition forClass: className isMeta: isMeta on: aStream [

	aStream
		nextPutAll: 'category: ', methodDefinition protocol printString; lf;
		nextPutAll: (isMeta ifTrue: ['classmethod: '] ifFalse: ['method: ']), className; lf;
		nextPutAll: methodDefinition source.
	methodDefinition source last = Character lf
		ifFalse: [aStream lf].
	aStream nextPutAll: '%'; lf;
		lf
]

{ #category : 'private exporting' }
RwAbstractGsModificationTopazWriterVisitorV2 >> _fileoutRemoveAllMethodsFor: className on: aStream [
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
RwAbstractGsModificationTopazWriterVisitorV2 >> _setBufferedStreamFor: filename extension: extension [

	| encodedStream |
	encodedStream := (self repositoryRootPath / filename, extension) writeStreamEncoded: 'utf8'.
	encodedStream truncate.
	bufferedStream := ZnBufferedWriteStream on: encodedStream
]

{ #category : 'accessing' }
RwAbstractGsModificationTopazWriterVisitorV2 >> bufferedStream [

	^ bufferedStream ifNil: [ 
		| encodedStream |
		encodedStream := (self repositoryRootPath / self topazFilename, self filenameExtension) writeStreamEncoded: 'utf8'.
		bufferedStream := ZnBufferedWriteStream on: encodedStream ]
]

{ #category : 'accessing' }
RwAbstractGsModificationTopazWriterVisitorV2 >> classDefinitions [

	^ classDefinitions ifNil: [ classDefinitions := Dictionary new ]
]

{ #category : 'accessing' }
RwAbstractGsModificationTopazWriterVisitorV2 >> classExtensions [

	^ classExtensions ifNil: [ classExtensions := Dictionary new ]
]

{ #category : 'accessing' }
RwAbstractGsModificationTopazWriterVisitorV2 >> classInitializationDefinitions [

	^ classInitializationDefinitions
]

{ #category : 'accessing' }
RwAbstractGsModificationTopazWriterVisitorV2 >> classSymbolDictionaryNames [

	^ classSymbolDictionaryNames ifNil: [ classSymbolDictionaryNames := Dictionary new ]
]

{ #category : 'accessing' }
RwAbstractGsModificationTopazWriterVisitorV2 >> excludeClassInitializers [
	^ excludeClassInitializers ifNil: [ false ]
]

{ #category : 'accessing' }
RwAbstractGsModificationTopazWriterVisitorV2 >> excludeClassInitializers: aBool [
	excludeClassInitializers := aBool
]

{ #category : 'accessing' }
RwAbstractGsModificationTopazWriterVisitorV2 >> excludeRemoveAllMethods [
	^ excludeRemoveAllMethods ifNil: [ false ]
]

{ #category : 'accessing' }
RwAbstractGsModificationTopazWriterVisitorV2 >> excludeRemoveAllMethods: aBool [
	excludeRemoveAllMethods := aBool
]

{ #category : 'exporting' }
RwAbstractGsModificationTopazWriterVisitorV2 >> exportClassInitializations [
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
		stream nextPutAll: classDef name, ' initialize.'; lf 
  ].
  exclude ifFalse:[
	  stream nextPutAll: 'true'; lf;
	  nextPutAll: '%'; lf .
  ].
]

{ #category : 'exporting' }
RwAbstractGsModificationTopazWriterVisitorV2 >> exportExtensionMethodDefinitions: classDefinitionsInOrder [

	self exportMethodDefinitions: classDefinitionsInOrder labeled: 'Class extensions'
]

{ #category : 'exporting' }
RwAbstractGsModificationTopazWriterVisitorV2 >> exportMethodDefinitions: classDefinitionsInOrder [

	self exportMethodDefinitions: classDefinitionsInOrder labeled: 'Class implementation'
]

{ #category : 'exporting' }
RwAbstractGsModificationTopazWriterVisitorV2 >> exportMethodDefinitions: classDefinitionsInOrder labeled: label [
	| stream |
	stream := self bufferedStream.
	classDefinitionsInOrder do: [:classDef | 
		| className |
		className := classDef name.
		(classDef classMethodDefinitions isEmpty not or: [ classDef instanceMethodDefinitions isEmpty not])
			ifTrue: [ 
				stream nextPutAll: '! ', label, ' for ', className printString; lf;
				lf ].
		classDef classMethodDefinitions isEmpty not
			ifTrue: [ 
				stream nextPutAll: '!		Class methods for ', className printString; lf;
				lf ].
		(classDef classMethodDefinitions values sort: [:a :b | a selector <= b selector ])
			do: [:methodDef |
				methodDef selector == #initialize
					ifTrue: [ self classInitializationDefinitions add: classDef ].
				self _fileOutMethod: methodDef forClass: className isMeta: true on: stream ].
		classDef instanceMethodDefinitions isEmpty not
			ifTrue: [ 
				stream nextPutAll: '!		Instance methods for ', className printString; lf;
				lf ].
		(classDef instanceMethodDefinitions values sort: [:a :b | a selector <= b selector ])
			do: [:methodDef |
				self _fileOutMethod: methodDef forClass: className isMeta: false on: stream ] ].
]

{ #category : 'accessing' }
RwAbstractGsModificationTopazWriterVisitorV2 >> filenameExtension [

	^ filenameExtension ifNil: ['gs' ]
]

{ #category : 'accessing' }
RwAbstractGsModificationTopazWriterVisitorV2 >> filenameExtension: aString [

	filenameExtension := aString
]

{ #category : 'accessing' }
RwAbstractGsModificationTopazWriterVisitorV2 >> logCreation [
	^logCreation ifNil: [ ^false ]
]

{ #category : 'accessing' }
RwAbstractGsModificationTopazWriterVisitorV2 >> logCreation: object [
	logCreation := object
]

{ #category : 'accessing' }
RwAbstractGsModificationTopazWriterVisitorV2 >> topazFileFooter [
	^ topazFileFooter ifNil: [ '' ]
]

{ #category : 'accessing' }
RwAbstractGsModificationTopazWriterVisitorV2 >> topazFileFooter: object [
	topazFileFooter := object
]

{ #category : 'accessing' }
RwAbstractGsModificationTopazWriterVisitorV2 >> topazFileHeader [

	^ topazFileHeader ifNil: [ '' ]
]

{ #category : 'accessing' }
RwAbstractGsModificationTopazWriterVisitorV2 >> topazFileHeader: aString [

	topazFileHeader := aString
]
