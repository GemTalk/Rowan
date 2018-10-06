Class {
	#name : 'RwGsModificationTopazWriterVisitor',
	#superclass : 'RwModificationWriterVisitor',
	#instVars : [
		'topazFilename',
		'filenameExtension',
		'currentPackageDefinition',
		'classDefinitions',
		'classExtensions',
		'bufferedStream'
	],
	#category : 'Rowan-GemStone-Core'
}

{ #category : 'private exporting' }
RwGsModificationTopazWriterVisitor >> _fileOutClassDeclaration: classDefinition on: aStream [
  aStream
    nextPutAll: 'doit';
    lf;
    nextPutAll: '(' , classDefinition superclassName;
    lf.
  self
    _writeClassTypeMessage: classDefinition
    on: aStream
    hasInstanceVariables: [ aStream
        nextPutAll:
            '	instVarNames: #( ' , (self _stringForVariables: classDefinition instVarNames) , ' )';
        lf ].
  aStream
    nextPutAll: '	classVars: #( ' ,  (self _stringForVariables: classDefinition classVarNames) , ' )';
    lf;
    nextPutAll:
        '	classInstVars: #( ' ,  (self _stringForVariables: classDefinition classInstVarNames) , ' )';
    lf;
    nextPutAll: '	poolDictionaries: #()';
    lf;
    nextPutAll: '	inDictionary: UserGlobals';
    lf;
    nextPutAll: '	options: #())';
    lf;
    nextPutAll: '		category: ' , classDefinition category printString , ';';
    lf;
    nextPutAll: '		comment: ' , classDefinition comment printString , ';';
    lf;
    nextPutAll: '		immediateInvariant.';
    lf;
    nextPutAll: 'true.';
    lf;
    nextPutAll: '%';
    lf;
    lf
]

{ #category : 'private exporting' }
RwGsModificationTopazWriterVisitor >> _fileOutMethod: methodDefinition forClass: className isMeta: isMeta on: aStream [

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
RwGsModificationTopazWriterVisitor >> _stringForVariables: variableList [

	| stream |
	stream := WriteStreamPortable on: (String new: 100).
	variableList do: [:each | stream nextPutAll: each]
		separatedBy: [stream space].
	^stream contents
]

{ #category : 'private exporting' }
RwGsModificationTopazWriterVisitor >> _writeClassTypeMessage: classDefinition on: aStream hasInstanceVariables: instanceVariableBlock [
  | classType classTypeMessage hasInstanceVariables |
  hasInstanceVariables := true.
  classType := classDefinition subclassType.
  classType = 'variable'
    ifTrue: [ classTypeMessage := 'indexableSubclass: ' ]
    ifFalse: [ classType = 'byteSubclass'
        ifTrue: [ classTypeMessage := 'byteSubclass: '.
          hasInstanceVariables := false ]
        ifFalse: [ classType = ''
            ifTrue: [ classTypeMessage := 'subclass: ' ]
            ifFalse: [ self error: 'unknown subclass type: ' , classType ] ] ].
  aStream
    tab;
    nextPutAll: classTypeMessage , classDefinition name asString printString;
    lf.
  hasInstanceVariables
    ifTrue: [ instanceVariableBlock value ]
]

{ #category : 'actions' }
RwGsModificationTopazWriterVisitor >> addedClass: aClassModification [

	self processClass: aClassModification
]

{ #category : 'actions' }
RwGsModificationTopazWriterVisitor >> addedClassExtension: aClassExtensionModification [

	self processClassExtension: aClassExtensionModification
]

{ #category : 'actions' }
RwGsModificationTopazWriterVisitor >> addedPackage: aPackageModification [

	self processPackage: aPackageModification
]

{ #category : 'actions' }
RwGsModificationTopazWriterVisitor >> addedProject: aProjectModification [

	| addedProject |
	super addedProject: aProjectModification.
	addedProject := aProjectModification after.
	aProjectModification packagesModification acceptVisitor: self
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitor >> bufferedStream [

	^ bufferedStream ifNil: [ 
		| encodedStream |
		encodedStream := (self repositoryRootPath / self topazFilename, self filenameExtension) writeStreamEncoded: 'utf8'.
		bufferedStream := ZnBufferedWriteStream on: encodedStream ]
]

{ #category : 'actions' }
RwGsModificationTopazWriterVisitor >> changedClass: aClassModification [

	self processClass: aClassModification
]

{ #category : 'actions' }
RwGsModificationTopazWriterVisitor >> changedClassExtension: aClassExtensionModification [

	self processClassExtension: aClassExtensionModification
]

{ #category : 'actions' }
RwGsModificationTopazWriterVisitor >> changedPackage: aPackageModification [

	self processPackage: aPackageModification
]

{ #category : 'actions' }
RwGsModificationTopazWriterVisitor >> changedProject: aProjectModification [

	super changedProject: aProjectModification.
self halt.

	aProjectModification packagesModification acceptVisitor: self
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitor >> classDefinitions [

	^ classDefinitions ifNil: [ classDefinitions := Dictionary new ]
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitor >> classExtensions [

	^ classExtensions ifNil: [ classExtensions := Dictionary new ]
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitor >> currentPackageDefinition [

	^ currentPackageDefinition
]

{ #category : 'actions' }
RwGsModificationTopazWriterVisitor >> deletededClass: aClassModification [

	"a deleted class is ignored for topaz ... the deleted class is simply not written"
]

{ #category : 'actions' }
RwGsModificationTopazWriterVisitor >> deletededClassExtension: aClassExtensionModification [

	"a deleted class extension is ignored for topaz ... the deleted methods are simply not written"
]

{ #category : 'actions' }
RwGsModificationTopazWriterVisitor >> deletededPackage: aPackageModification [

	"a deleted package is ignored for topaz ... the deleted classes and methods are simply not written out"

self halt. "if we end up writing out per package topaz files, then something significant needs to happen here"
]

{ #category : 'project writing' }
RwGsModificationTopazWriterVisitor >> ensureProjectDirectoryStructureFor: aProjectDefinition [

	"topaz files do not go in a standard spot in the project directory structure. The files are written directly in the repositoryRootPath"
]

{ #category : 'exporting' }
RwGsModificationTopazWriterVisitor >> export [

	| classDefinitionsInOrder classExtensionsInOrder classExtensionsList |
	classDefinitionsInOrder := (CypressGsGeneralDependencySorter
		on: self classDefinitions values
		dependsOn: [:candidate | candidate superclassName]
		dependent: [:candidate | candidate name]) inOrder.
	self exportClassDefinitions: classDefinitionsInOrder;
		exportMethodDefinitions: classDefinitionsInOrder.

	"consolidate the classExtensions for a class from multiple packages into a single definition"
	classExtensionsList := Set new.
	self classExtensions values do: [:aSet |
		aSet size <= 1 
			ifTrue: [ classExtensionsList addAll: aSet ]
			ifFalse: [
				| ar def |
				ar := aSet asArray.
				def := (ar at: 1) copy.
				2 to: ar size do: [:index |
					| d |
					d := (ar at: index).
					d classMethodDefinitions values do: [:m | def addClassMethodDefinition: m ].
					d instanceMethodDefinitions values do: [:m | def addInstanceMethodDefinition: m ] ].
				classExtensionsList add: def ] ].

	classExtensionsInOrder := (CypressGsGeneralDependencySorter
		on: classExtensionsList
		dependsOn: [:candidate | 'Object' ]
		dependent: [:candidate | candidate name ]) inOrder.
	self
		exportExtensionMethodDefinitions: classExtensionsInOrder.

	self bufferedStream flush; close
]

{ #category : 'exporting' }
RwGsModificationTopazWriterVisitor >> exportClassDefinitions: classDefinitionsInOrder [

	| stream |
	stream := self bufferedStream.
	classDefinitionsInOrder isEmpty not
		ifTrue: [ 
			stream
				nextPutAll: '! Class Declarations'; lf;
				lf ].
	classDefinitionsInOrder do: [:classDef | 
		self _fileOutClassDeclaration: classDef on: stream ].
]

{ #category : 'exporting' }
RwGsModificationTopazWriterVisitor >> exportExtensionMethodDefinitions: classDefinitionsInOrder [

	self exportMethodDefinitions: classDefinitionsInOrder labeled: 'Class extensions'
]

{ #category : 'exporting' }
RwGsModificationTopazWriterVisitor >> exportMethodDefinitions: classDefinitionsInOrder [

	self exportMethodDefinitions: classDefinitionsInOrder labeled: 'Class implementation'
]

{ #category : 'exporting' }
RwGsModificationTopazWriterVisitor >> exportMethodDefinitions: classDefinitionsInOrder labeled: label [

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
RwGsModificationTopazWriterVisitor >> filenameExtension [

	^ filenameExtension ifNil: ['gs' ]
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitor >> filenameExtension: aString [

	filenameExtension := aString
]

{ #category : 'class writing' }
RwGsModificationTopazWriterVisitor >> processClass: aClassModification [

	| classDefinition |
	classDefinition := aClassModification after.
	(self classDefinitions at: classDefinition name ifAbsent: []) ifNotNil: [ self error: 'duplicate class defintion for ', classDefinition name printString, ' encountered.'].
	self classDefinitions at: classDefinition name put: classDefinition.

	"no need to visit any further as the class definition records the instance and class methods"
]

{ #category : 'class writing' }
RwGsModificationTopazWriterVisitor >> processClassExtension: aClassExtensionModification [

	| classExtension |
	classExtension := aClassExtensionModification after.
	(self classExtensions at: classExtension name ifAbsentPut: [ Set new ])
		add: classExtension

	"no need to visit any further as the class etension records the instance and class methods"
]

{ #category : 'package writing' }
RwGsModificationTopazWriterVisitor >> processPackage: aPackageModification [

	currentPackageDefinition := aPackageModification after.
	aPackageModification classesModification acceptVisitor: self.
	aPackageModification classExtensionsModification acceptVisitor: self
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitor >> topazFilename [

	^ topazFilename
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitor >> topazFilename: aString [

	topazFilename := aString
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitor >> topazFilename: aNameString filenameExtension: anExtensionString [

	topazFilename := aNameString.
	filenameExtension := anExtensionString
]

{ #category : 'accessing' }
RwGsModificationTopazWriterVisitor >> topazFileReference [

	^ self repositoryRootPath / self topazFilename, self filenameExtension
]
