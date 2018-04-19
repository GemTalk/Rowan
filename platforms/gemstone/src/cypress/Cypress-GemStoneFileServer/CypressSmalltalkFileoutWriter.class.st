"
All Cypress classes are private to GemStone and are likely to be removed in a future release.
"
Class {
	#name : 'CypressSmalltalkFileoutWriter',
	#superclass : 'CypressAbstractFileoutWriter',
	#category : 'Cypress-GemStoneFileServer'
}

{ #category : 'writing - private' }
CypressSmalltalkFileoutWriter >> fileOut: aString methodsPreambleFor: classStructure on: aStream [

	self
		writeChunk: '" ------------------- ' , aString , ' methods for '
				, classStructure name , '"'
		on: aStream.
	aStream
		lf;
		lf

]

{ #category : 'writing - private' }
CypressSmalltalkFileoutWriter >> fileOutClassDeclaration: classStructure on: aStream [
	"This is the structure for VW/Pharo  class definitions. It may or may not be portable
	to all dialects that use chunk-format fileins. It does not attempt to handle VW
	namespaces at this time. Probably should use an XML fileout for that."

	aStream
		nextPutAll: classStructure superclassName;
		nextPutAll: ' subclass: #';
		nextPutAll: classStructure className;
		lf;
		nextPutAll: '	instanceVariableNames:  ''';
		nextPutAll: classStructure instanceVariablesString;
		nextPut: $';
		lf;
		nextPutAll: '	classVariableNames: ''';
		nextPutAll: classStructure classVariablesString;
		nextPut: $';
		lf;
		nextPutAll: '	poolDictionaries: ''';
		nextPutAll: classStructure poolDictionariesString;
		nextPut: $';
		lf;
		nextPutAll: '	category: ''';
		nextPutAll: classStructure category.
	self writeChunk: '''' on: aStream.
	aStream
		lf;
		lf;
		nextPutAll: classStructure className;
		nextPutAll: ' comment:';
		lf.
	self writeChunk: classStructure comment printString on: aStream.
	aStream
		lf;
		lf;
		nextPutAll: classStructure className;
		nextPutAll: ' class instanceVariableNames: ''';
		nextPutAll: classStructure classInstanceVariablesString.
	self writeChunk: '''' on: aStream.
	aStream
		lf;
		lf

]

{ #category : 'writing - private' }
CypressSmalltalkFileoutWriter >> fileOutClassesPreambleOn: aStream [

	self writeChunk: '" Class Declarations "' on: aStream.
	aStream
		lf;
		lf

]

{ #category : 'writing - private' }
CypressSmalltalkFileoutWriter >> fileOutClassInitializerFor: classStructure on: aStream [

	self writeChunk: classStructure className , ' initialize.' on: aStream.
	aStream lf

]

{ #category : 'writing - private' }
CypressSmalltalkFileoutWriter >> fileOutClassInitializersPostambleOn: aStream [

	aStream
		lf

]

{ #category : 'writing - private' }
CypressSmalltalkFileoutWriter >> fileOutClassInitializersPreambleOn: aStream [

	self writeChunk: '" Class initializers "' on: aStream.
	aStream
		lf;
		lf

]

{ #category : 'writing - private' }
CypressSmalltalkFileoutWriter >> fileOutExtensionsPreambleOn: aStream [

	self writeChunk: '" Class Extensions "' on: aStream.
	aStream
		lf;
		lf

]

{ #category : 'writing - private' }
CypressSmalltalkFileoutWriter >> fileOutMethod: methodStructure on: aStream [

	aStream nextPutAll: '!'.
	self
		writeChunk: methodStructure classStructure className
				, (methodStructure isMetaclass
						ifTrue: [' class methodsFor: ']
						ifFalse: [' methodsFor: '])
					, methodStructure category printString
		on: aStream.
	aStream lf.
	self
		writeChunk: methodStructure source on: aStream;
		writeChunk: ' ' on: aStream.
	aStream lf

]

{ #category : 'writing - private' }
CypressSmalltalkFileoutWriter >> fileOutPackagePostambleOn: aStream [

	aStream
		lf;
		lf.
	self writeChunk: '" End of Package: ' , self packageName , '"' on: aStream.
	aStream
		lf;
		lf;
		lf

]

{ #category : 'writing - private' }
CypressSmalltalkFileoutWriter >> fileOutPackagePreambleOn: aStream [

	self writeChunk: '" Package: ' , self packageName , '"' on: aStream.
	aStream
		lf;
		lf;
		lf

]

{ #category : 'writing - private' }
CypressSmalltalkFileoutWriter >> fileOutPreambleType: aString for: classStructure on: aStream [

	self writeChunk: '" ' , aString , ' for ' , classStructure name , '"'
		on: aStream.
	aStream
		lf;
		lf

]

{ #category : 'accessing' }
CypressSmalltalkFileoutWriter >> packageNameExtension [

	^'.st'

]

{ #category : 'writing - private' }
CypressSmalltalkFileoutWriter >> writeChunk: aString on: aStream [

	aString do: 
			[:each |
			aStream nextPut: each.
			each = $! ifTrue: [aStream nextPut: each]].
	aStream nextPut: $!

]
