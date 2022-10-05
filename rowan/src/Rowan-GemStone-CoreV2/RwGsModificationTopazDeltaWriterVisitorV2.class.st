Class {
	#name : 'RwGsModificationTopazDeltaWriterVisitorV2',
	#superclass : 'RwAbstractGsModificationTopazWriterVisitorV2',
	#instVars : [
		'repositoryRootPath',
		'topazFileNameMap',
		'currentFileName',
		'deltaFilenameMap'
	],
	#category : 'Rowan-GemStone-CoreV2'
}

{ #category : '_private' }
RwGsModificationTopazDeltaWriterVisitorV2 class >> _topazFileNameMap: resolvedProject [
	| gemStoneRowanPackageNames filein4PackageNames combinedPackageNames topazFileNameMap |
	gemStoneRowanPackageNames := #('GemStone-Rowan-Extensions-Tools' 'GemStone-Rowan-Tools').
	filein4PackageNames := #('Filein4-CompilerClasses' 'Filein4-ObsoleteClasses' 'Filein4Rowan').
	combinedPackageNames := gemStoneRowanPackageNames , filein4PackageNames.
	topazFileNameMap := Dictionary new
		at: 'Filein4' put: filein4PackageNames asSet;
		at: 'GemStone-Rowan' put: gemStoneRowanPackageNames asSet;
		yourself.
	resolvedProject packageNames
		do: [ :packageName | 
			(combinedPackageNames includes: packageName)
				ifFalse: [ 
					"write one package per file, except for the GemStone-Rowan and Filein4 packages. NOTE: GemStone-Rowan written out separately below"
					(topazFileNameMap at: packageName ifAbsentPut: [ {} ]) add: packageName ] ].
	^ topazFileNameMap
]

{ #category : 'private' }
RwGsModificationTopazDeltaWriterVisitorV2 >> _currentClassName [
	self currentClassDefinition ifNotNil: [ :def | ^ def name ].
	^ self currentClassExtension name
]

{ #category : 'private' }
RwGsModificationTopazDeltaWriterVisitorV2 >> _fileNameForPackageName: packageName [
	self topazFileNameMap
		keysAndValuesDo: [ :fileName :packageNameSet | 
			(packageNameSet includes: packageName)
				ifTrue: [ ^ fileName ] ].
	self error: 'No filename found for ' , packageName printString
]

{ #category : 'private exporting' }
RwGsModificationTopazDeltaWriterVisitorV2 >> _setBufferedStreamFor: filename [

	^ self _setBufferedStreamFor: filename extension: self filenameExtension
]

{ #category : 'private' }
RwGsModificationTopazDeltaWriterVisitorV2 >> _topazFileNameMap: resolvedProject [
	topazFileNameMap := self class _topazFileNameMap: resolvedProject
]

{ #category : 'actions' }
RwGsModificationTopazDeltaWriterVisitorV2 >> addedMethod: aMethodModification [
	| methodDefinition source theStream |
	theStream := self bufferedStream.
	methodDefinition := aMethodModification after.
	theStream
		nextPutAll: 'category: ' , methodDefinition protocol;
		lf.
	aMethodModification isMeta
		ifTrue: [ theStream nextPutAll: 'classmethod:  ' ]
		ifFalse: [ theStream nextPutAll: 'method: ' ].
	theStream
		nextPutAll: self _currentClassName;
		lf.
	source := methodDefinition source.
	theStream nextPutAll: source.
	source last = Character lf
		ifFalse: [ theStream lf ].
	theStream
		nextPutAll: '%';
		lf
]

{ #category : 'actions' }
RwGsModificationTopazDeltaWriterVisitorV2 >> addedMethodExtension: aMethodExtensionModification [
	self addedMethod: aMethodExtensionModification
]

{ #category : 'accessing' }
RwGsModificationTopazDeltaWriterVisitorV2 >> bufferedStream [
	| packageName fileName |
	packageName := currentPackageDefinition name.
	fileName := self _fileNameForPackageName: packageName.
	deltaFilenameMap ifNil: [ deltaFilenameMap := Dictionary new ].
	bufferedStream := deltaFilenameMap
		at: fileName
		ifAbsentPut: [ 
			self _setBufferedStreamFor: fileName extension: self filenameExtension.
			bufferedStream
				nextPutAll: '! BEGIN delta file ' , fileName , '.' , self filenameExtension;
				lf;
				nextPutAll: self topazFileHeader;
				lf.
			fileName = 'Filein1A'
				ifTrue: [
					bufferedStream
						nextPutAll: 'input $upgradeFileinDir/Filein1A-BootstrapOnly.gs'; lf ].
			bufferedStream ].
	^ bufferedStream
]

{ #category : 'actions' }
RwGsModificationTopazDeltaWriterVisitorV2 >> changedMethod: aMethodModification [
	self addedMethod: aMethodModification
]

{ #category : 'actions' }
RwGsModificationTopazDeltaWriterVisitorV2 >> changedMethodExtension: aMethodExtensionModification [
	self addedMethodExtension: aMethodExtensionModification
]

{ #category : 'accessing' }
RwGsModificationTopazDeltaWriterVisitorV2 >> currentClassDefinition: object [
	currentClassExtension := nil.
	currentClassDefinition := object
]

{ #category : 'accessing' }
RwGsModificationTopazDeltaWriterVisitorV2 >> currentClassExtension: object [
	currentClassDefinition := nil.
	currentClassExtension := object
]

{ #category : 'actions' }
RwGsModificationTopazDeltaWriterVisitorV2 >> deletedClass: aClassModification [
	"deleted classes are handled directly by upgradeImage ... remove all Methods for now"

	| classDefinition theStream |
	theStream := self bufferedStream.
	classDefinition := aClassModification before.
	theStream
		nextPutAll: 'removeallmethods ';
		nextPutAll: classDefinition name;
		lf;
		nextPutAll: 'removeallclassmethods ';
		nextPutAll: classDefinition name;
		lf;
		lf
]

{ #category : 'actions' }
RwGsModificationTopazDeltaWriterVisitorV2 >> deletedClassExtension: aClassExtensionModification [
	"noop"
]

{ #category : 'actions' }
RwGsModificationTopazDeltaWriterVisitorV2 >> deletedMethod: aMethodModification [
	| methodDefinition theStream |
	theStream := self bufferedStream.
	methodDefinition := aMethodModification before.
	theStream
		nextPutAll: 'expectvalue true';
		lf;
		nextPutAll: 'run';
		lf;
		nextPutAll: self _currentClassName;
		space.
	aMethodModification isMeta
		ifTrue: [ theStream nextPutAll: 'class ' ].
	theStream
		nextPutAll: 'removeSelector: #''' , methodDefinition selector, ''' ifAbsent: [].';
		lf;
		nextPutAll: 'true';
		lf;
		nextPutAll: '%';
		lf
]

{ #category : 'actions' }
RwGsModificationTopazDeltaWriterVisitorV2 >> deletedMethodExtension: aMethodExtensionModification [
	self deletedMethod: aMethodExtensionModification
]

{ #category : 'actions' }
RwGsModificationTopazDeltaWriterVisitorV2 >> deletedPackage: aPackageModification [
	aPackageModification before name = 'Filein1D-ObsoleteClasses'
		ifTrue: [ 
			"expected"
			^ self ].
	self
		error:
			'Unexpected package deletion: ' , aPackageModification before name printString
]

{ #category : 'exporting' }
RwGsModificationTopazDeltaWriterVisitorV2 >> export [
	"close the files now that we're done"

	deltaFilenameMap
		keysAndValuesDo: [ :fileName :stream | 
			stream
				nextPutAll: '! END delta file ' , fileName , '.' , self filenameExtension;
				lf;
				flush;
				close ]
]

{ #category : 'class writing' }
RwGsModificationTopazDeltaWriterVisitorV2 >> processClass: aClassModification [
	"we do not process class modifications for classes with kernel oop, since they are updated in bom.c"

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
	self _topazFileNameMap: aProjectModification after.
	super processProject: aProjectModification
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

{ #category : 'accessing' }
RwGsModificationTopazDeltaWriterVisitorV2 >> topazFileNameMap [
	^topazFileNameMap
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
