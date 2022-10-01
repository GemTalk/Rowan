Class {
	#name : 'RwGsModificationTopazDeltaWriterVisitorV2',
	#superclass : 'RwAbstractGsModificationTopazWriterVisitorV2',
	#instVars : [
		'deltaFile',
		'deltaFileStream',
		'repositoryRootPath'
	],
	#category : 'Rowan-GemStone-CoreV2'
}

{ #category : 'private' }
RwGsModificationTopazDeltaWriterVisitorV2 >> _currentClassName [
	self currentClassDefinition ifNotNil: [ :def | ^ def name ].
	^ self currentClassExtension name
]

{ #category : 'private exporting' }
RwGsModificationTopazDeltaWriterVisitorV2 >> _setBufferedStreamFor: filename [

	^ self _setBufferedStreamFor: filename extension: self filenameExtension
]

{ #category : 'actions' }
RwGsModificationTopazDeltaWriterVisitorV2 >> addedMethod: aMethodModification [
	| methodDefinition source |
	methodDefinition := aMethodModification after.
	bufferedStream
		nextPutAll: 'category: ' , methodDefinition protocol;
		lf.
	aMethodModification isMeta
		ifTrue: [ bufferedStream nextPutAll: 'classmethod:  ' ]
		ifFalse: [ bufferedStream nextPutAll: 'method: ' ].
	bufferedStream
		nextPutAll: self _currentClassName;
		lf.
	source := methodDefinition source.
	bufferedStream nextPutAll: source.
	source last = Character lf
		ifFalse: [ bufferedStream lf ].
	bufferedStream
		nextPutAll: '%';
		lf
]

{ #category : 'actions' }
RwGsModificationTopazDeltaWriterVisitorV2 >> addedMethodExtension: aMethodExtensionModification [
	self addedMethod: aMethodExtensionModification
]

{ #category : 'actions' }
RwGsModificationTopazDeltaWriterVisitorV2 >> changedMethod: aMethodModification [
	self addedMethod: aMethodModification
]

{ #category : 'actions' }
RwGsModificationTopazDeltaWriterVisitorV2 >> changedMethodExtension: aMethodExtensionModification [
	self addedMethodExtension: aMethodExtensionModification
]

{ #category : 'actions' }
RwGsModificationTopazDeltaWriterVisitorV2 >> deletedClass: aClassModification [
	"deleted classes are handled directly by upgradeImage ... remove all Methods for now"

	| classDefinition |
	classDefinition := aClassModification before.
	bufferedStream
		nextPutAll: 'removeallmethods ';
		nextPutAll: classDefinition name;
		lf;
		nextPutAll: 'removeallclassmethods ';
		nextPutAll: classDefinition name;
		lf
]

{ #category : 'actions' }
RwGsModificationTopazDeltaWriterVisitorV2 >> deletedClassExtension: aClassExtensionModification [
	"noop"
]

{ #category : 'actions' }
RwGsModificationTopazDeltaWriterVisitorV2 >> deletedMethod: aMethodModification [
	| methodDefinition |
	methodDefinition := aMethodModification before.
	bufferedStream
		nextPutAll: 'run';
		lf;
		nextPutAll: self _currentClassName;
		space.
	aMethodModification isMeta
		ifTrue: [ bufferedStream nextPutAll: 'class ' ].
	bufferedStream
		nextPutAll: 'removeSelector: #''' , methodDefinition selector, '''';
		lf;
		nextPutAll: '%';
		lf
]

{ #category : 'actions' }
RwGsModificationTopazDeltaWriterVisitorV2 >> deletedMethodExtension: aMethodExtensionModification [
	self deletedMethod: aMethodExtensionModification
]

{ #category : 'accessing' }
RwGsModificationTopazDeltaWriterVisitorV2 >> deltaFile [
	^deltaFile
]

{ #category : 'accessing' }
RwGsModificationTopazDeltaWriterVisitorV2 >> deltaFile: object [
	deltaFile := object
]

{ #category : 'accessing' }
RwGsModificationTopazDeltaWriterVisitorV2 >> filenameExtension [

	^ filenameExtension ifNil: ['tpz' ]
]

{ #category : 'class writing' }
RwGsModificationTopazDeltaWriterVisitorV2 >> processClass: aClassModification [
	"we do not process class modifications directly. As a rule, we do not create new class versions,
		during image filein, but we do need to process class category and method changes"

	aClassModification instanceMethodsModification acceptVisitor: self.
	aClassModification classMethodsModification acceptVisitor: self.
]

{ #category : 'class extension writing' }
RwGsModificationTopazDeltaWriterVisitorV2 >> processClassExtension: aClassExtensionModification [

	aClassExtensionModification instanceMethodsModification acceptVisitor: self.
	aClassExtensionModification classMethodsModification acceptVisitor: self.
]

{ #category : 'project writing' }
RwGsModificationTopazDeltaWriterVisitorV2 >> processProject: aProjectModification [
	[ 
	self _setBufferedStreamFor: self deltaFile.
	self bufferedStream nextPutAll: self topazFileHeader.
	super processProject: aProjectModification.
	self bufferedStream nextPutAll: self topazFileFooter ]
		ensure: [ 
			self bufferedStream
				flush;
				close ]
]

{ #category : 'accessing' }
RwGsModificationTopazDeltaWriterVisitorV2 >> repositoryRootPath [

	^ repositoryRootPath
]

{ #category : 'accessing' }
RwGsModificationTopazDeltaWriterVisitorV2 >> repositoryRootPath: aStringOrFileReference [ 

	repositoryRootPath := aStringOrFileReference asFileReference.
	repositoryRootPath exists ifFalse: [ self error: 'The repository root path must exist: ', repositoryRootPath pathString printString ]
]

{ #category : 'visiting' }
RwGsModificationTopazDeltaWriterVisitorV2 >> visitExtensionMethodModification: aMethodExtensionModification [

	aMethodExtensionModification isAddition
		ifTrue: [ ^ self addedMethodExtension: aMethodExtensionModification ] .
	aMethodExtensionModification isDeletion
		ifTrue: [ ^ self deletedMethodExtension: aMethodExtensionModification ].
	^ self changedMethodExtension: aMethodExtensionModification
]

{ #category : 'visiting' }
RwGsModificationTopazDeltaWriterVisitorV2 >> visitExtensionMethodsModification: aMethodExtensionsModification [

	aMethodExtensionsModification elementsModified
		do: [ :each | each acceptVisitor: self ]
]

{ #category : 'visiting' }
RwGsModificationTopazDeltaWriterVisitorV2 >> visitMethodModification: aMethodModification [

	aMethodModification isAddition
		ifTrue: [ ^ self addedMethod: aMethodModification ] .
	aMethodModification isDeletion
		ifTrue: [ ^ self deletedMethod: aMethodModification ].
	^ self changedMethod: aMethodModification
]

{ #category : 'visiting' }
RwGsModificationTopazDeltaWriterVisitorV2 >> visitMethodsModification: aMethodsModification [

	aMethodsModification elementsModified
		do: [:each | each acceptVisitor: self]
]
