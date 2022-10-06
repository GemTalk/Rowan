Class {
	#name : 'RwGsModificationTopazPackageWriterVisitorV2',
	#superclass : 'RwAbstractGsModificationTrackingTopazWriterVisitorV2',
	#instVars : [
		'packagesRoot'
	],
	#category : 'Rowan-GemStone-CoreV2'
}

{ #category : 'private exporting' }
RwGsModificationTopazPackageWriterVisitorV2 >> _setBufferedStreamFor: filename [

	^ self _setBufferedStreamFor: filename extension: self filenameExtension
]

{ #category : 'private exporting' }
RwGsModificationTopazPackageWriterVisitorV2 >> _setBufferedStreamFor: filename extension: extension [
	bufferedStream := FileStreamPortable
		write: (self packagesRoot / filename , extension) pathString
		mode: #'append'
		check: false
		type: #serverText
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
	(self classDefinitions at: (clsName := classDefinition name) ifAbsent: []) ifNotNil: [ 
   self error: 'duplicate class definition for ', clsName printString, ' encountered.'].

	symbolDictName := self currentProjectDefinition symbolDictNameForPackageNamed: self currentPackageDefinition name.
	self classSymbolDictionaryNames at: classDefinition name put: symbolDictName.
	self classDefinitions at: classDefinition name put: classDefinition.

	"no need to visit any further as the class definition records the instance and class methods"
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
