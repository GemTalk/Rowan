Class {
	#name : 'RwGsModificationTopazDeltaWriterVisitorV2',
	#superclass : 'RwAbstractGsModificationTrackingTopazWriterVisitorV2',
	#instVars : [
		'repositoryRootPath',
		'topazFileNameMap',
		'deltaFilenameMap',
		'perFileDefinitionsMap',
		'addMethodDefinitions',
		'deleteMethodDefinitions',
		'deleteClassDefinitions',
		'classReferenceDefinitions',
		'reportStream'
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

{ #category : 'private' }
RwGsModificationTopazDeltaWriterVisitorV2 >> _resetState [
	"clear the instance vars and get ready for the next package"

	super _resetState.
	addMethodDefinitions := deleteMethodDefinitions := deleteClassDefinitions := nil
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
	(self addMethodDefinitions
		at: self currentClassOrExtensionDefinition name
		ifAbsentPut: [ Set new ]) add: aMethodModification
]

{ #category : 'actions' }
RwGsModificationTopazDeltaWriterVisitorV2 >> addedMethodExtension: aMethodExtensionModification [
	self addedMethod: aMethodExtensionModification
]

{ #category : 'accessing' }
RwGsModificationTopazDeltaWriterVisitorV2 >> addMethodDefinitions [
	^ addMethodDefinitions ifNil: [ addMethodDefinitions := Dictionary new ]
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
RwGsModificationTopazDeltaWriterVisitorV2 >> classInitializationDefinitions [
	^ classInitializationDefinitions
		ifNil: [ classInitializationDefinitions := Set new ]
]

{ #category : 'accessing' }
RwGsModificationTopazDeltaWriterVisitorV2 >> classReferenceDefinitions [
	^ classReferenceDefinitions
		ifNil: [ classReferenceDefinitions := Dictionary new ]
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

{ #category : 'accessing' }
RwGsModificationTopazDeltaWriterVisitorV2 >> currentClassOrExtensionDefinition [
	^ currentClassExtension ifNil: [ currentClassDefinition ]
]

{ #category : 'package writing' }
RwGsModificationTopazDeltaWriterVisitorV2 >> definitionsMapFor: filename [
	^ self perFileDefinitionsMap
		at: filename
		ifAbsentPut: [ 
			Dictionary new
				at: 'classDefinitions' put: Dictionary new;
				at: 'classReferenceDefinitions' put: Dictionary new;
				at: 'deleteClassDefinitions' put: Dictionary new;
				at: 'classInitializationDefinitions' put: Set new;
				at: 'classSymbolDictionaryNames' put: Dictionary new;
				at: 'addMethodDefinitions' put: Dictionary new;
				at: 'deleteMethodDefinitions' put: Dictionary new;
				yourself ]
]

{ #category : 'accessing' }
RwGsModificationTopazDeltaWriterVisitorV2 >> deleteClassDefinitions [
	^ deleteClassDefinitions ifNil: [ deleteClassDefinitions := Dictionary new ]
]

{ #category : 'actions' }
RwGsModificationTopazDeltaWriterVisitorV2 >> deletedClass: aClassModification [
	(self deleteClassDefinitions
		at: self currentClassDefinition name
		ifAbsentPut: [ Set new ]) add: aClassModification
]

{ #category : 'actions' }
RwGsModificationTopazDeltaWriterVisitorV2 >> deletedClassExtension: aClassExtensionModification [
	"noop"
]

{ #category : 'actions' }
RwGsModificationTopazDeltaWriterVisitorV2 >> deletedMethod: aMethodModification [
	(self deleteMethodDefinitions
		at: self currentClassOrExtensionDefinition name
		ifAbsentPut: [ Set new ]) add: aMethodModification
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

{ #category : 'accessing' }
RwGsModificationTopazDeltaWriterVisitorV2 >> deleteMethodDefinitions [
	^ deleteMethodDefinitions ifNil: [ deleteMethodDefinitions := Dictionary new ]
]

{ #category : 'accessing' }
RwGsModificationTopazDeltaWriterVisitorV2 >> excludeRemoveAllMethods [
	^ excludeRemoveAllMethods ifNil: [ true ]
]

{ #category : 'exporting' }
RwGsModificationTopazDeltaWriterVisitorV2 >> export [
	"create a redirection File for Filein1A-BootstrapOnly.gs and Filein3B-BootstrapOnly.gs in the delta directory"

	{'Filein1A-BootstrapOnly'.
	'Filein3B-BootstrapOnly'}
		do: [ :bootstrapOnlyFileName | 
			self
				_setBufferedStreamFor: bootstrapOnlyFileName
				extension: self filenameExtension.
			bufferedStream
				nextPutAll:
						'! DELTA redirect file ' , bootstrapOnlyFileName , self filenameExtension;
				lf;
				lf;
				nextPutAll:
						'input $upgradeFileinDir/../../' , bootstrapOnlyFileName , '.'
								, self filenameExtension;
				lf.
			bufferedStream flush close ].

	self topazFileNameMap keys sort
		do: [ :filename | 
			| definitionMap classDefinitionsList classDefinitionsInOrder classDefs |
			bufferedStream
				nextPutAll: 'Exporting ' , filename;
				lf;
				nextPutAll: '--------------------';
				lf.

			definitionMap := self definitionsMapFor: filename.
			classSymbolDictionaryNames := definitionMap at: 'classSymbolDictionaryNames'.

			self _setBufferedStreamFor: filename extension: self filenameExtension.
			bufferedStream
				nextPutAll: '! DELTA ' , filename , '.' , self filenameExtension;
				lf;
				nextPutAll: self topazFileHeader;
				lf.
			filename = 'Filein1A'
				ifTrue: [ 
					bufferedStream
						nextPutAll: 'input $upgradeFileinDir/../../Filein1A-BootstrapOnly.gs';
						lf;
						lf ].

			classDefs := definitionMap at: 'classDefinitions'.
			classDefinitionsList := classDefs values.
			classDefinitionsList
				addAll: (definitionMap at: 'classReferenceDefinitions') values.

			classDefinitionsInOrder := (RowanGsGeneralDependencySorter
				on: classDefinitionsList
				dependsOn: [ :candidate | candidate superclassName ]
				dependent: [ :candidate | candidate name ]) inOrder.
			self
				exportClassDefinitions: classDefinitionsInOrder
				forClassDefinitions: classDefs.

			self
				exportMethodAdd: (definitionMap at: 'addMethodDefinitions')
				methodDelete: (definitionMap at: 'deleteMethodDefinitions').

			bufferedStream flush close ].
	self reportStream flush close
]

{ #category : 'exporting' }
RwGsModificationTopazDeltaWriterVisitorV2 >> exportAddedMethod: aMethodModification [
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
		nextPutAll: aMethodModification classDefinition name;
		lf.
	source := methodDefinition source.
	theStream nextPutAll: source.
	source last = Character lf
		ifFalse: [ theStream lf ].
	theStream
		nextPutAll: '%';
		lf;
		flush
]

{ #category : 'exporting' }
RwGsModificationTopazDeltaWriterVisitorV2 >> exportClassDefinitions: classDefinitionsInOrder forClassDefinitions: classDefs [
	| stream |
	stream := self bufferedStream.
	stream
		nextPutAll: '! Class Changes';
		lf.
	self reportStream
		tab;
		nextPutAll: 'Exporting Class Changes';
		lf.
	classDefinitionsInOrder
		do: [ :classDef | 
			(classDefs includesKey: classDef name)
				ifTrue: [ 
					self reportStream
						tab;
						tab;
						nextPutAll: classDef name;
						lf.
					self _fileOutClassDeclaration: classDef on: stream ] ]
]

{ #category : 'exporting' }
RwGsModificationTopazDeltaWriterVisitorV2 >> exportDeletedClass: aClassModification [
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

{ #category : 'exporting' }
RwGsModificationTopazDeltaWriterVisitorV2 >> exportDeletedMethod: aMethodModification [
	| methodDefinition theStream |
	theStream := self bufferedStream.
	methodDefinition := aMethodModification before.
	theStream
		nextPutAll: 'run';
		lf;
		nextPutAll: aMethodModification classDefinition name;
		space.
	aMethodModification isMeta
		ifTrue: [ theStream nextPutAll: 'class ' ].
	theStream
		nextPutAll: 'removeSelector: #''' , methodDefinition selector , '''.';
		lf;
		nextPutAll: 'true';
		lf;
		nextPutAll: '%';
		lf;
		flush
]

{ #category : 'exporting' }
RwGsModificationTopazDeltaWriterVisitorV2 >> exportMethodAdd: addMethods methodDelete: deleteMethods [
	| methodMods |
	self bufferedStream
		nextPutAll: '! Method Changes';
		lf.
	methodMods := Set new.
	addMethods values do: [ :methodSet | methodMods addAll: methodSet ].
	self reportStream
		tab;
		nextPutAll: 'Changed methods';
		lf.
	methodMods
		do: [ :methodMod | 
			| className methodDef methodString |
			methodDef := methodMod after.
			className := methodMod classDefinition name.
			methodString := className
				,
					(methodMod isMeta
						ifTrue: [ ' class >> ' ]
						ifFalse: [ ' >> ' ]) , methodDef selector.
			self reportStream
				tab;
				tab;
				nextPutAll: methodString;
				lf.
			self exportAddedMethod: methodMod ].
	methodMods := Set new.
	deleteMethods values do: [ :methodSet | methodMods addAll: methodSet ].
	self reportStream
		tab;
		nextPutAll: 'Deleted methods';
		lf.
	methodMods
		do: [ :methodMod | 
			| className methodDef methodString |
			methodDef := methodMod before.
			className := methodMod classDefinition name.
			methodString := className
				,
					(methodMod isMeta
						ifTrue: [ ' class >> ' ]
						ifFalse: [ ' >> ' ]) , methodDef selector.
			self reportStream
				tab;
				tab;
				nextPutAll: methodString;
				lf.
			self exportDeletedMethod: methodMod ]
]

{ #category : 'accessing' }
RwGsModificationTopazDeltaWriterVisitorV2 >> perFileDefinitionsMap [
	^ perFileDefinitionsMap ifNil: [ perFileDefinitionsMap := Dictionary new ]
]

{ #category : 'class writing' }
RwGsModificationTopazDeltaWriterVisitorV2 >> processClass: aClassModification [
	| classDefinition symbolDictName clsName |
	"class definition needs to be processed ... if reserved oop is prosent, bom.c manages class changes"
	classDefinition := aClassModification after.
	(classDefinition gs_reservedOop isEmpty
		and: [ 
			"if no changes to class definition don't write out class definition"
			aClassModification propertiesModification elementsModified isEmpty not ])
		ifTrue: [ 
			(self classDefinitions at: (clsName := classDefinition name) ifAbsent: [  ])
				ifNotNil: [ 
					self
						error:
							'duplicate class definition for ' , clsName printString , ' encountered.' ].
			symbolDictName := self currentProjectDefinition
				symbolDictNameForPackageNamed: self currentPackageDefinition name.
			self classSymbolDictionaryNames at: classDefinition name put: symbolDictName.
			self classDefinitions at: classDefinition name put: classDefinition ]
		ifFalse: [ 
			"record class references for ordering purposes"
			self classReferenceDefinitions at: classDefinition name put: classDefinition ].

	aClassModification instanceMethodsModification acceptVisitor: self.
	aClassModification classMethodsModification acceptVisitor: self
]

{ #category : 'class extension writing' }
RwGsModificationTopazDeltaWriterVisitorV2 >> processClassExtension: aClassExtensionModification [

	aClassExtensionModification instanceMethodsModification acceptVisitor: self.
	aClassExtensionModification classMethodsModification acceptVisitor: self.
]

{ #category : 'package writing' }
RwGsModificationTopazDeltaWriterVisitorV2 >> processPackage: aPackageModification [
	| definitionsMap definitionMap |
	super processPackage: aPackageModification.
	definitionsMap := self
		definitionsMapFor:
			(self _fileNameForPackageName: aPackageModification after name).

	definitionMap := definitionsMap at: 'classDefinitions'.
	self classDefinitions
		keysAndValuesDo: [ :className :classDefinition | 
			(definitionMap at: className ifAbsent: [  ])
				ifNotNil: [ 
					self
						error:
							'duplicate class definition for ' , className printString , ' encountered.' ].
			definitionMap at: className put: classDefinition ].

	definitionMap := definitionsMap at: 'classReferenceDefinitions'.
	self classReferenceDefinitions
		keysAndValuesDo: [ :className :classDefinition | definitionMap at: className put: classDefinition ].

	definitionMap := definitionsMap at: 'classSymbolDictionaryNames'.
	self classSymbolDictionaryNames
		keysAndValuesDo: [ :className :symbolDictName | definitionMap at: className put: symbolDictName ].

	definitionMap := definitionsMap at: 'deleteClassDefinitions'.
	self deleteClassDefinitions
		keysAndValuesDo: [ :className :classDefinition | 
			(definitionMap at: className ifAbsent: [  ])
				ifNotNil: [ 
					self
						error:
							'duplicate delete class definition for ' , className printString
								, ' encountered.' ].
			definitionMap at: className put: classDefinition ].

	(definitionsMap at: 'classInitializationDefinitions')
		addAll: self classInitializationDefinitions.

	definitionMap := definitionsMap at: 'addMethodDefinitions'.
	self addMethodDefinitions
		keysAndValuesDo: [ :className :methodDefs | (definitionMap at: className ifAbsentPut: [ Set new ]) addAll: methodDefs ].

	definitionMap := definitionsMap at: 'deleteMethodDefinitions'.
	self deleteMethodDefinitions
		keysAndValuesDo: [ :className :methodDefs | (definitionMap at: className ifAbsentPut: [ Set new ]) addAll: methodDefs ].

	self _resetState
]

{ #category : 'project writing' }
RwGsModificationTopazDeltaWriterVisitorV2 >> processProject: aProjectModification [
	self _topazFileNameMap: aProjectModification after.
	super processProject: aProjectModification
]

{ #category : 'accessing' }
RwGsModificationTopazDeltaWriterVisitorV2 >> reportStream [
	^ reportStream
		ifNil: [ 
			| encodedStream |
			encodedStream := self repositoryRootPath / 'DeltaReport.txt'
				writeStreamEncoded: 'utf8'.
			reportStream := ZnBufferedWriteStream on: encodedStream ]
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
RwGsModificationTopazDeltaWriterVisitorV2 >> visitClassModification: aClassModification [
	aClassModification isAddition
		ifTrue: [ ^ self addedClass: aClassModification ].
	aClassModification isDeletion
		ifTrue: [ ^ self deletedClass: aClassModification ].
	(aClassModification instanceMethodsModification isEmpty
		and: [ aClassModification classMethodsModification isEmpty ])
		ifTrue: [ 
			| propertiesModification "no instance or class modifications, check to see if the only Property modification is for 'gs_SymbolDictionary' " elementsModified |
			propertiesModification := aClassModification propertiesModification.
			elementsModified := propertiesModification elementsModified.
			(elementsModified size = 1
				and: [ (elementsModified at: 'gs_SymbolDictionary' ifAbsent: [  ]) notNil ])
				ifTrue: [ 
					"'gs_symbolDictionary' property is not written to disk, so we can skip this class"
					^ self ] ].
	^ self changedClass: aClassModification
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
