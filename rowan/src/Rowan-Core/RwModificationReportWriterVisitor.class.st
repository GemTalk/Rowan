"
This class produces a report report describing the changes that would be made to the image (base) if project1 is loaded into  an image containing project 2.

	| diff visitor label1 label2 loadSpec project1 project2  fileReference |
	loadSpec := RwSpecification fromUrl: 'https://raw.githubusercontent.com/dalehenrich/RowanSample9V3/refs/heads/spec_0001/rowan/specs/spec_0001.ston'.
	loadSpec
		projectAlias: loadSpec projectName, '_1';
		projectsHome: '$ROWAN_PROJECTS_HOME';
		repositoryResolutionPolicy: #'strict';
		yourself .
	project1 := loadSpec read.
	loadSpec := RwSpecification fromUrl: 'https://raw.githubusercontent.com/dalehenrich/RowanSample9V3/refs/heads/spec_0002/rowan/specs/spec_0002.ston'.
	loadSpec
		projectAlias: loadSpec projectName, '_2';
		projectsHome: '$ROWAN_PROJECTS_HOME';
		repositoryResolutionPolicy: #'strict';
		yourself .
	project2 := loadSpec read.
	label1 := 'spec_0001'.
	label2:= 'spec_0002'.
	diff := project2 compareAgainstBase: project1.
	visitor := RwModificationReportWriterVisitor new
		excludeClassInitializers: false;
	 	excludeRemoveAllMethods: false;
	 	repositoryRootPath: '/bosch1/users/dhenrich/_stones/37x/stones/rowan3_dev_i/reports' asFileReference;
	 	reportFilename: label2, '_into_', label1,'.txt';
	 	reportFileHeader:'REPORT ', label2, ' loading into ', label1, '
';
	 	reportFileFooter: '_____
';
	 	yourself.
	visitor visit: diff.

====== Report ======
REPORT spec_0002 loading into spec_0001
 -- Added Packages --
        RowanSample9V3-Extensions
        RowanSample9V3-GemStone-Tests
        RowanSample9V3-GemStone
 -- Added Classes --
        RowanSample9V3IdentityKeyValueDictionary
        RowanSample9V3GemStoneTestCase
_____
"
Class {
	#name : 'RwModificationReportWriterVisitor',
	#superclass : 'RwModificationWriterVisitor',
	#instVars : [
		'reportFilename',
		'reportFileHeader',
		'reportFileFooter',
		'excludeClassInitializers',
		'excludeRemoveAllMethods',
		'filenameExtension',
		'classSymbolDictionaryNames',
		'classDefinitions',
		'classExtensions',
		'traitDefinitions',
		'bufferedStream',
		'addedClasses',
		'changedClasses',
		'deletedClasses',
		'addedClassExtensions',
		'changedClassExtensions',
		'deletedClassExtensions',
		'addedPackages',
		'changedPackages',
		'deletedPackages',
		'addedProjects',
		'changedProjects',
		'deletedProjects',
		'addedTraits',
		'changedTraits',
		'deletedTraits',
		'addedMethods',
		'changedMethods',
		'changedMethodExtensions',
		'deletedMethods',
		'addedTraitMethods',
		'changedTraitMethods',
		'deletedTraitMethods',
		'classDefPackageNameMap',
		'classExtPackageNameMap',
		'classInitializationDefinitions',
		'repositoryRootPath',
		'filterTrailingLf'
	],
	#classVars : [
		'Character_lf'
	],
	#category : 'Rowan-Core'
}

{ #category : 'initialization' }
RwModificationReportWriterVisitor class >> initialize [
	self _addInvariantClassVar: #Character_lf value: Character lf
]

{ #category : 'private exporting' }
RwModificationReportWriterVisitor >> _bufferedStream: aZnBufferedWriteStream [
	bufferedStream := aZnBufferedWriteStream
]

{ #category : 'private exporting' }
RwModificationReportWriterVisitor >> _fileOutClassDeclaration: classDefinition on: aStream [
	| optionsString reservedOopString hasClassInstVars |
	aStream
		nextPutAll: 'doit';
		lf;
		nextPutAll: '(' ; nextPutAll: classDefinition superclassName;
		lf.
	hasClassInstVars := self
		_writeClassTypeMessage: classDefinition
		on: aStream
		hasInstanceVariables: [ 
			aStream
				nextPutAll: '	instVarNames: #(' .
      self _stringForVariables: classDefinition instVarNames to: aStream .
      aStream nextPutAll: ')' ; lf ].
	optionsString := String new.
	classDefinition gs_options isEmpty
		ifFalse: [ 
			optionsString := String new .
      self _symbolsForVariables: classDefinition gs_options toString: optionsString .
		  ].
	self logCreation
		ifTrue: [ 
			"for verbose logging during filein and upgrade"
			optionsString addAll: ' #logCreation ' ].
	reservedOopString := String new .
	classDefinition gs_reservedOop isEmpty
		ifFalse: [ 
			reservedOopString addAll: '	reservedOop: ' ; addAll: classDefinition gs_reservedOop;
				lf.
		].
	aStream
		nextPutAll: '	classVars: #(' . 
  self _stringForVariables: classDefinition classVarNames to: aStream .
  aStream nextPutAll:')' ; lf .
	hasClassInstVars
		ifTrue: [ 
			aStream nextPutAll: '	classInstVars: #(' .
			self _stringForVariables: classDefinition classInstVarNames to: aStream .
      aStream nextPutAll:')' ; lf 
    ].
	aStream
		nextPutAll: '	poolDictionaries: #()';
		lf;
		nextPutAll: '	inDictionary: ' ; nextPutAll: (self classSymbolDictionaryNames at: classDefinition name);
		lf;
		nextPutAll: '	options: #(' ; nextPutAll: optionsString ; nextPut: $) ;
		lf;
		nextPutAll: reservedOopString;
		nextPutAll: ')';
		lf;
		nextPutAll: '		category: '; nextPutAll: classDefinition category printString ; nextPut: $; ;
		lf .
	classDefinition comment size == 0
		ifFalse: [ 
			aStream
				nextPutAll: '		comment: '; nextPutAll: classDefinition comment printString; nextPut: $; ;
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
RwModificationReportWriterVisitor >> _fileOutMethod: methodDefinition forClass: className isMeta: isMeta on: aStream [

	aStream
		nextPutAll: 'category: '; nextPutAll: methodDefinition protocol printString; lf;
		nextPutAll: (isMeta ifTrue: ['classmethod: '] ifFalse: ['method: ']) ; nextPutAll: className; lf;
		nextPutAll: methodDefinition source.
	methodDefinition source last == Character_lf
		ifFalse: [aStream lf].
	aStream nextPut: $% ; lf;
		lf
]

{ #category : 'private exporting' }
RwModificationReportWriterVisitor >> _fileOutMethod: methodDefinition forTrait: traitName isMeta: isMeta on: aStream [

	aStream
		nextPutAll: 'category: '; nextPutAll: methodDefinition protocol printString; lf;
		nextPutAll: (isMeta ifTrue: ['trclassmethod: '] ifFalse: ['trmethod: ']) ; nextPutAll: traitName; lf;
		nextPutAll: methodDefinition source.
	methodDefinition source last == Character_lf
		ifFalse: [aStream lf].
	aStream nextPut: $% ; lf;
		lf
]

{ #category : 'private exporting' }
RwModificationReportWriterVisitor >> _fileoutRemoveAllMethodsFor: className on: aStream [
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
RwModificationReportWriterVisitor >> _fileOutTraitDeclaration: traitDefinition on: aStream [
	aStream
		nextPutAll: 'doit';
		lf;
		nextPutAll: '(Trait';
		lf;
		nextPutAll: '	name: ' , traitDefinition name printString;
		lf;
		nextPutAll: '	instVars: #('.
	self _stringForVariables: traitDefinition instVarNames to: aStream.
	aStream
		nextPutAll: ')';
		lf.
	aStream nextPutAll: '	classVars: #('.
	self _stringForVariables: traitDefinition classVarNames to: aStream.
	aStream
		nextPutAll: ')';
		lf.
	aStream nextPutAll: '	classInstVars: #('.
	self _stringForVariables: traitDefinition classInstVarNames to: aStream.
	aStream
		nextPutAll: ')';
		lf.
	aStream
		nextPutAll: '	inDictionary: ';
		nextPutAll: (self classSymbolDictionaryNames at: traitDefinition name);
		lf;
		nextPutAll: ')';
		lf;
		nextPutAll: '		category: ';
		nextPutAll: traitDefinition category printString.	"
	traitDefinition comment size == 0
		ifFalse: [ 
			aStream
				nextput: $;
				lf;
				nextPutAll: '		comment: ';
				nextPutAll: traitDefinition comment printString;
				yourself ].
"
	aStream
		nextPut: $.;
		lf.
	aStream
		nextPutAll: 'true.';
		lf;
		nextPutAll: '%';
		lf.
	self _fileoutTraitRemoveAllMethodsFor: traitDefinition name on: aStream
]

{ #category : 'private exporting' }
RwModificationReportWriterVisitor >> _fileoutTraitRemoveAllMethodsFor: traitName on: aStream [
	self excludeRemoveAllMethods
		ifFalse: [ 
			aStream
				nextPutAll: 'trremoveallmethods ' , traitName;
				lf;
				nextPutAll: 'trremoveallclassmethods ' , traitName;
				lf;
				lf ]
]

{ #category : 'private exporting' }
RwModificationReportWriterVisitor >> _setBufferedStreamFor: filename [

	^ self _setBufferedStreamFor: filename extension: self filenameExtension
]

{ #category : 'private exporting' }
RwModificationReportWriterVisitor >> _setBufferedStreamFor: filename extension: extension [

	| encodedStream |
	encodedStream := (self repositoryRootPath / filename, extension) writeStreamEncoded: 'utf8'.
	encodedStream truncate.
	bufferedStream := ZnBufferedWriteStream on: encodedStream
]

{ #category : 'private exporting' }
RwModificationReportWriterVisitor >> _stringForVariables: variableList to: aStream [
	| leadingSpace |
	leadingSpace := false.
	variableList
		do: [ :each | 
			leadingSpace
				ifTrue: [ aStream nextPut: $  ]
				ifFalse: [ leadingSpace := true ].
			aStream nextPutAll: each ]
]

{ #category : 'private exporting' }
RwModificationReportWriterVisitor >> _symbolsForVariables: variableList toString: aString [
	| trailingSpace |
	trailingSpace := false.
	variableList
		do: [ :each | 
			trailingSpace := true.
			aString
				add: $ ;
				addAll: each asSymbol printString ].
	trailingSpace
		ifTrue: [ aString add: $  ]
]

{ #category : 'private exporting' }
RwModificationReportWriterVisitor >> _writeClassTypeMessage: classDefinition on: aStream hasInstanceVariables: instanceVariableBlock [
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
		nextPutAll: classTypeMessage ; nextPutAll: classDefinition name asString printString;
		lf.
	hasInstanceVariables
		ifTrue: [ instanceVariableBlock value ].
	^ hasClassInstVars
]

{ #category : 'actions' }
RwModificationReportWriterVisitor >> addedClass: aClassModification [
	currentClassDefinition := aClassModification after.
	self addedClasses add: currentClassDefinition.
	self processClass: aClassModification
]

{ #category : 'accessing' }
RwModificationReportWriterVisitor >> addedClasses [ 

^ addedClasses ifNil: [ addedClasses := Set new]
]

{ #category : 'actions' }
RwModificationReportWriterVisitor >> addedClassExtension: aClassExtensionModification [
	currentClassExtension := aClassExtensionModification after.
	self addedClassExtensions add: aClassExtensionModification after
]

{ #category : 'accessing' }
RwModificationReportWriterVisitor >> addedClassExtensions [ 

^ addedClassExtensions ifNil: [ addedClassExtensions := Set new]
]

{ #category : 'actions' }
RwModificationReportWriterVisitor >> addedMethod: aMethodModification [
	self addedMethods
		add:
			currentClassDefinition name
				,
					(aMethodModification isMeta
						ifTrue: [ ' class>>' ]
						ifFalse: [ '>>' ]) , aMethodModification after selector
]

{ #category : 'accessing' }
RwModificationReportWriterVisitor >> addedMethods [
	^ addedMethods ifNil: [ addedMethods := Set new ]
]

{ #category : 'actions' }
RwModificationReportWriterVisitor >> addedPackage: aPackageModification [
	self addedPackages add: aPackageModification after.
	self processPackage: aPackageModification
]

{ #category : 'accessing' }
RwModificationReportWriterVisitor >> addedPackages [
	^ addedPackages ifNil: [ addedPackages := Set new ]
]

{ #category : 'actions' }
RwModificationReportWriterVisitor >> addedProject: aProjectModification [
	self addedProjects add: aProjectModification after.
	self processProject: aProjectModification
]

{ #category : 'accessing' }
RwModificationReportWriterVisitor >> addedProjects [
	^ addedProjects ifNil: [ addedProjects := Set new ]
]

{ #category : 'actions' }
RwModificationReportWriterVisitor >> addedTrait: aTraitModification [
	self addedTraits add:  aTraitModification after name
]

{ #category : 'actions' }
RwModificationReportWriterVisitor >> addedTraitMethod: aTraitMethodModification [
	self addedTraitMethods
		add:
			currentTraitDefinition name
				,
					(aTraitMethodModification isMeta
						ifTrue: [ ' class>>' ]
						ifFalse: [ '>>' ]) , aTraitMethodModification after selector
]

{ #category : 'accessing' }
RwModificationReportWriterVisitor >> addedTraitMethods [
	^ addedTraitMethods ifNil: [ addedTraitMethods := Set new ]
]

{ #category : 'accessing' }
RwModificationReportWriterVisitor >> addedTraits [
	^ addedTraits ifNil: [ addedTraits := Set new ]
]

{ #category : 'accessing' }
RwModificationReportWriterVisitor >> bufferedStream [

	^ bufferedStream ifNil: [ 
		| encodedStream |
		encodedStream := (self repositoryRootPath / self topazFilename, self filenameExtension) writeStreamEncoded: 'utf8'.
		bufferedStream := ZnBufferedWriteStream on: encodedStream ]
]

{ #category : 'actions' }
RwModificationReportWriterVisitor >> changedClass: aClassModification [
	currentClassDefinition := aClassModification after.
	(aClassModification isModification
		and: [ aClassModification propertiesModification isEmpty not ])
		ifTrue: [ self changedClasses add: currentClassDefinition ].
	self processClass: aClassModification
]

{ #category : 'accessing' }
RwModificationReportWriterVisitor >> changedClasses [

	^changedClasses ifNil: [ changedClasses := Set new ]
]

{ #category : 'actions' }
RwModificationReportWriterVisitor >> changedClassExtension: aClassExtensionModification [
	currentClassExtension := aClassExtensionModification after.
	aClassExtensionModification isModification
		ifTrue: [ self changedClassExtensions add: currentClassExtension ].
	self processClassExtension: aClassExtensionModification
]

{ #category : 'accessing' }
RwModificationReportWriterVisitor >> changedClassExtensions [
	^ changedClassExtensions ifNil: [ changedClassExtensions := Set new ]
]

{ #category : 'actions' }
RwModificationReportWriterVisitor >> changedMethod: aMethodModification [
	(self checkForSourceDiffWithoutTrailingLf: aMethodModification)
		ifTrue: [ 
			"ignore the methodModification if source differs only by a trailing LF"
			^ self ].
	self changedMethods
		add:
			currentClassDefinition name
				,
					(aMethodModification isMeta
						ifTrue: [ ' class>>' ]
						ifFalse: [ '>>' ]) , aMethodModification after selector
]

{ #category : 'actions' }
RwModificationReportWriterVisitor >> changedMethodExtension: aMethodModification [
	(self checkForSourceDiffWithoutTrailingLf: aMethodModification)
		ifTrue: [ 
			"ignore the methodModification source differs only by a trailing LF"
			^ self ].
	self changedMethodExtensions
		add:
			currentClassExtension name
				,
					(aMethodModification isMeta
						ifTrue: [ ' class>>' ]
						ifFalse: [ '>>' ]) , aMethodModification after selector
]

{ #category : 'accessing' }
RwModificationReportWriterVisitor >> changedMethodExtensions [
	^ changedMethodExtensions ifNil: [ changedMethodExtensions := Set new ]
]

{ #category : 'accessing' }
RwModificationReportWriterVisitor >> changedMethods [
	^ changedMethods ifNil: [ changedMethods := Set new ]
]

{ #category : 'actions' }
RwModificationReportWriterVisitor >> changedPackage: aPackageModification [
	currentPackageDefinition := aPackageModification after.
	aPackageModification isModification
		ifTrue: [ self changedPackages add: currentPackageDefinition ].
	self processPackage: aPackageModification
]

{ #category : 'accessing' }
RwModificationReportWriterVisitor >> changedPackages [ 

^ changedPackages ifNil: [ changedPackages := Set new]
]

{ #category : 'actions' }
RwModificationReportWriterVisitor >> changedProject: aProjectModification [
	currentProjectDefinition := aProjectModification after.
	aProjectModification isModification
		ifTrue: [ self changedProjects add: currentProjectDefinition ].
	self processProject: aProjectModification
]

{ #category : 'accessing' }
RwModificationReportWriterVisitor >> changedProjects [ 

^ changedProjects ifNil: [ changedProjects := Set new]
]

{ #category : 'actions' }
RwModificationReportWriterVisitor >> changedTrait: aTraitModification [

	currentTraitDefinition := aTraitModification after.
	self changedTraits add: currentTraitDefinition.
	self processTrait: aTraitModification
]

{ #category : 'actions' }
RwModificationReportWriterVisitor >> changedTraitMethod: aMethodTraitModification [
	(self checkForSourceDiffWithoutTrailingLf: aMethodTraitModification)
		ifTrue: [ 
			"ignore the methodModification if source differs only by a trailing LF"
			^ self ].
	self changedTraitMethods
		add:
			currentTraitDefinition name
				,
					(aMethodTraitModification isMeta
						ifTrue: [ ' class>>' ]
						ifFalse: [ '>>' ]) , aMethodTraitModification after selector
]

{ #category : 'accessing' }
RwModificationReportWriterVisitor >> changedTraitMethods [
	^ changedTraitMethods ifNil: [ changedTraitMethods := Set new ]
]

{ #category : 'actions' }
RwModificationReportWriterVisitor >> changedTraits [

	^changedTraits ifNil: [ changedTraits := Set new ]
]

{ #category : 'private' }
RwModificationReportWriterVisitor >> checkForSourceDiffWithoutTrailingLf: aMethodModification [
	"Answer true if the only difference  for the method is  a trailing lf"

	| beforeSource afterSource afterHasLastLf beforeHasLastLf before after |
	aMethodModification propertiesModification isEmpty
		ifFalse: [ 
			"properties modification is present, so trailing lf is not a deciding factor"
			^ false ].
	self filterTrailingLf
		ifFalse: [ 
			"not filtering for trailing lf"
			^ false ].
	before := beforeSource := aMethodModification before source.
	after := afterSource := aMethodModification after source.
	afterHasLastLf := beforeHasLastLf := false.
	beforeSource last = Character lf
		ifTrue: [ 
			"remove trailing lf"
			beforeHasLastLf := true.
			before := beforeSource copyFrom: 1 to: beforeSource size - 1 ].
	afterSource last = Character lf
		ifTrue: [ 
			"remove trailing lf"
			afterHasLastLf := true.
			after := afterSource copyFrom: 1 to: afterSource size - 1 ].
	afterHasLastLf == beforeHasLastLf
		ifTrue: [ 
			"if they do or do not have  a trailing lf, then the original comparison applies -- the method is changed"
			^ false ].
	^ after = before	"answer whether or not the source (wihtout trailing lf) is equal or not"
]

{ #category : 'accessing' }
RwModificationReportWriterVisitor >> classDefinitions [

	^ classDefinitions ifNil: [ classDefinitions := Dictionary new ]
]

{ #category : 'accessing' }
RwModificationReportWriterVisitor >> classDefPackageNameMap [

	^ classDefPackageNameMap ifNil: [ classDefPackageNameMap := Dictionary new ]
]

{ #category : 'accessing' }
RwModificationReportWriterVisitor >> classExtensions [

	^ classExtensions ifNil: [ classExtensions := Dictionary new ]
]

{ #category : 'accessing' }
RwModificationReportWriterVisitor >> classExtPackageNameMap [

	^ classExtPackageNameMap ifNil: [ classExtPackageNameMap := IdentityKeyValueDictionary new ]
]

{ #category : 'accessing' }
RwModificationReportWriterVisitor >> classInitializationDefinitions [

	^ classInitializationDefinitions
]

{ #category : 'accessing' }
RwModificationReportWriterVisitor >> classSymbolDictionaryNames [

	^ classSymbolDictionaryNames ifNil: [ classSymbolDictionaryNames := Dictionary new ]
]

{ #category : 'actions' }
RwModificationReportWriterVisitor >> deletedClass: aClassModification [
	self deletedClasses add: aClassModification before name
]

{ #category : 'accessing' }
RwModificationReportWriterVisitor >> deletedClasses [
	^ deletedClasses ifNil: [ deletedClasses := Set new ]
]

{ #category : 'actions' }
RwModificationReportWriterVisitor >> deletedClassExtension: aClassExtensionModification [
	self deletedClassExtensions add: aClassExtensionModification before name
]

{ #category : 'accessing' }
RwModificationReportWriterVisitor >> deletedClassExtensions [
	^ deletedClassExtensions ifNil: [ deletedClassExtensions := Set new ]
]

{ #category : 'actions' }
RwModificationReportWriterVisitor >> deletedMethod: aMethodModification [
	self deletedMethods
		add:
			currentClassDefinition name
				,
					(aMethodModification isMeta
						ifTrue: [ ' class>>' ]
						ifFalse: [ '>>' ]) , aMethodModification before selector
]

{ #category : 'accessing' }
RwModificationReportWriterVisitor >> deletedMethods [ 

^ deletedMethods ifNil: [ deletedMethods := Set new]
]

{ #category : 'actions' }
RwModificationReportWriterVisitor >> deletedPackage: aPackageModification [
	self deletedPackages add: aPackageModification before name
]

{ #category : 'accessing' }
RwModificationReportWriterVisitor >> deletedPackages [ 

^ deletedPackages ifNil: [ deletedPackages := Set new]
]

{ #category : 'actions' }
RwModificationReportWriterVisitor >> deletedProject: aProjectModification [
	self deletedProjects add: aProjectModification before name
]

{ #category : 'accessing' }
RwModificationReportWriterVisitor >> deletedProjects [ 

^ deletedProjects ifNil: [ deletedProjects := Set new]
]

{ #category : 'actions' }
RwModificationReportWriterVisitor >> deletedTraitMethod: aMethodTraitModification [
	self deletedTraitMethods
		add:
			currentTraitDefinition name
				,
					(aMethodTraitModification isMeta
						ifTrue: [ ' class>>' ]
						ifFalse: [ '>>' ]) , aMethodTraitModification before selector
]

{ #category : 'accessing' }
RwModificationReportWriterVisitor >> deletedTraitMethods [ 

^ deletedTraitMethods ifNil: [ deletedTraitMethods := Set new]
]

{ #category : 'accessing' }
RwModificationReportWriterVisitor >> deletedTraits [ 

^ deletedTraits ifNil: [ deletedTraits := Set new]
]

{ #category : 'accessing' }
RwModificationReportWriterVisitor >> excludeClassInitializers [
	^ excludeClassInitializers ifNil: [ false ]
]

{ #category : 'accessing' }
RwModificationReportWriterVisitor >> excludeClassInitializers: aBool [
	excludeClassInitializers := aBool
]

{ #category : 'accessing' }
RwModificationReportWriterVisitor >> excludeRemoveAllMethods [
	^ excludeRemoveAllMethods ifNil: [ false ]
]

{ #category : 'accessing' }
RwModificationReportWriterVisitor >> excludeRemoveAllMethods: aBool [
	excludeRemoveAllMethods := aBool
]

{ #category : 'accessing' }
RwModificationReportWriterVisitor >> filenameExtension [

	^ filenameExtension ifNil: ['txt' ]
]

{ #category : 'accessing' }
RwModificationReportWriterVisitor >> filenameExtension: aString [

	filenameExtension := aString
]

{ #category : 'accessing' }
RwModificationReportWriterVisitor >> filterTrailingLf [
	^ filterTrailingLf ifNil: [ filterTrailingLf := false ]
]

{ #category : 'accessing' }
RwModificationReportWriterVisitor >> filterTrailingLf: aBool [
	filterTrailingLf := aBool
]

{ #category : 'class writing' }
RwModificationReportWriterVisitor >> processClass: aClassModification [
	| classDefinition clsName |
	classDefinition := aClassModification after.
	self classDefinitions at: (clsName := classDefinition name) ifAbsent: [  
	self classDefinitions at: classDefinition name put: classDefinition].

	self classDefPackageNameMap
		at: classDefinition name
		put: currentPackageDefinition name.

	currentClassDefinition := classDefinition.
	currentClassDefinition instanceMethodDefinitions isEmpty
		ifTrue: [ 
			"nothing to do"
			 ]
		ifFalse: [ aClassModification instanceMethodsModification  acceptVisitor: self ].
	currentClassDefinition classMethodDefinitions isEmpty
		ifTrue: [ 
			"nothing to do"
			 ]
		ifFalse: [ aClassModification classMethodsModification  acceptVisitor: self ]
]

{ #category : 'class writing' }
RwModificationReportWriterVisitor >> processClassExtension: aClassExtensionModification [
	| classExtensionDefinition |
	classExtensionDefinition := aClassExtensionModification after.
	self classExtensions at: classExtensionDefinition name put: classExtensionDefinition.

	self classDefPackageNameMap
		at: classExtensionDefinition name
		put: currentPackageDefinition name.

	currentClassExtension instanceMethodDefinitions isEmpty
		ifTrue: [ 
			"nothing to do"
			 ]
		ifFalse: [ aClassExtensionModification instanceMethodsModification acceptVisitor: self ].
	currentClassExtension classMethodDefinitions isEmpty
		ifTrue: [ 
			"nothing to do"
			 ]
		ifFalse: [ aClassExtensionModification classMethodsModification acceptVisitor: self ]
]

{ #category : 'package writing' }
RwModificationReportWriterVisitor >> processPackage: aPackageModification [

	currentPackageDefinition := aPackageModification after.
	super processPackage: aPackageModification
]

{ #category : 'class writing' }
RwModificationReportWriterVisitor >> processTrait: aTraitModification [
	| traitDefinition traitName |
	traitDefinition := aTraitModification after.
	(self traitDefinitions at: (traitName := traitDefinition name) ifAbsent: [  ])
		ifNotNil: [ 
			self
				error:
					'duplicate trait definition for ' , traitName printString , ' encountered.' ].
	self classDefPackageNameMap
		at: traitDefinition name
		put: currentPackageDefinition name.

	currentTraitDefinition instanceMethodDefinitions isEmpty
		ifTrue: [ 
			"nothing to do"
			 ]
		ifFalse: [ aTraitModification instanceMethodsModification acceptVisitor: self ].
	currentTraitDefinition classMethodDefinitions isEmpty
		ifTrue: [ 
			"nothing to do"
			 ]
		ifFalse: [ aTraitModification classMethodsModification acceptVisitor: self ]
]

{ #category : 'reporting' }
RwModificationReportWriterVisitor >> report [
	bufferedStream ifNil: [ self _setBufferedStreamFor: self reportFilename ].
	self bufferedStream nextPutAll: self reportFileHeader.

	self deletedProjects isEmpty
		ifFalse: [ 
			self bufferedStream
				nextPutAll: ' -- Deleted Projects --';
				lf.
			self deletedProjects asSortedCollection
				do: [ :each | 
					self bufferedStream
						nextPutAll: '    ';
						nextPutAll: each;
						lf ] ].
	self addedProjects isEmpty
		ifFalse: [ 
			self bufferedStream
				nextPutAll: ' -- Added Projects --';
				lf.
			(self addedProjects asSortedCollection: [ :a :b | a name <= b name ])
				do: [ :each | 
					self bufferedStream
						nextPutAll: '    ';
						nextPutAll: each name;
						lf ] ].
	self changedProjects isEmpty
		ifFalse: [ 
			self bufferedStream
				nextPutAll: ' -- Changed Projects --';
				lf.
			(self changedProjects asSortedCollection: [ :a :b | a name <= b name ])
				do: [ :each | 
					self bufferedStream
						nextPutAll: '    ';
						nextPutAll: each name;
						lf ] ].
	self deletedPackages isEmpty
		ifFalse: [ 
			self bufferedStream
				nextPutAll: ' -- Deleted Packages --';
				lf.
			self deletedPackages asSortedCollection
				do: [ :each | 
					self bufferedStream
						nextPutAll: '    ';
						nextPutAll: each;
						lf ] ].
	self addedPackages isEmpty
		ifFalse: [ 
			self bufferedStream
				nextPutAll: ' -- Added Packages --';
				lf.
			(self addedPackages asSortedCollection: [ :a :b | a name <= b name ])
				do: [ :each | 
					self bufferedStream
						nextPutAll: '    ';
						nextPutAll: each name;
						lf ] ].
	self changedPackages isEmpty
		ifFalse: [ 
			self bufferedStream
				nextPutAll: ' -- Changed Packages --';
				lf.
			(self changedPackages asSortedCollection: [ :a :b | a name <= b name ])
				do: [ :each | 
					self bufferedStream
						nextPutAll: '    ';
						nextPutAll: each name;
						lf ] ].
	self deletedClasses isEmpty
		ifFalse: [ 
			self bufferedStream
				nextPutAll: ' -- Deleted Classes --';
				lf.
			self deletedClasses
				do: [ :each | 
					self bufferedStream
						nextPutAll: '    ';
						nextPutAll: each;
						lf ] ].
	self addedClasses isEmpty
		ifFalse: [ 
			self bufferedStream
				nextPutAll: ' -- Added Classes --';
				lf.
			(self addedClasses asSortedCollection: [ :a :b | a name <= b name ])
				do: [ :each | 
					self bufferedStream
						nextPutAll: '    ';
						nextPutAll: each name;
						lf ] ].
	self changedClasses isEmpty
		ifFalse: [ 
			self bufferedStream
				nextPutAll: ' -- Changed Classes --';
				lf.
			(self changedClasses asSortedCollection: [ :a :b | a name <= b name ])
				do: [ :each | 
					self bufferedStream
						nextPutAll: '    ';
						nextPutAll: each name;
						lf ] ].
	self addedClassExtensions isEmpty
		ifFalse: [ 
			self bufferedStream
				nextPutAll: ' -- Added Class Extensions --';
				lf.
			(self addedClassExtensions asSortedCollection: [ :a :b | a name <= b name ])
				do: [ :each | 
					self bufferedStream
						nextPutAll: '    ';
						nextPutAll: each name;
						lf ] ].
	self deletedClassExtensions isEmpty
		ifFalse: [ 
			self bufferedStream
				nextPutAll: ' -- Deleted Class Extensions --';
				lf.
			self deletedClassExtensions asSortedCollection
				do: [ :each | 
					self bufferedStream
						nextPutAll: '    ';
						nextPutAll: each;
						lf ] ].
	self deletedTraits isEmpty
		ifFalse: [ 
			self bufferedStream
				nextPutAll: ' -- Deleted Traits --';
				lf.
			self deletedTraits asSortedCollection
				do: [ :each | 
					self bufferedStream
						nextPutAll: '    ';
						nextPutAll: each;
						lf ] ].
	self addedTraits isEmpty
		ifFalse: [ 
			self bufferedStream
				nextPutAll: ' -- Added Traits --';
				lf.
			self addedTraits asSortedCollection
				do: [ :each | 
					self bufferedStream
						nextPutAll: '    ';
						nextPutAll: each;
						lf ] ].
	self changedTraits isEmpty
		ifFalse: [ 
			self bufferedStream
				nextPutAll: ' -- Changed Traits --';
				lf.
			(self changedTraits asSortedCollection: [ :a :b | a name <= b name ])
				do: [ :each | 
					self bufferedStream
						nextPutAll: '    ';
						nextPutAll: each name;
						lf ] ].
	self deletedMethods isEmpty
		ifFalse: [ 
			self bufferedStream
				nextPutAll: ' -- Deleted Methods --';
				lf.
			self deletedMethods asSortedCollection
				do: [ :each | 
					self bufferedStream
						nextPutAll: '    ';
						nextPutAll: each;
						lf ] ].
	self addedMethods isEmpty
		ifFalse: [ 
			self bufferedStream
				nextPutAll: ' -- Added Methods --';
				lf.
			self addedMethods asSortedCollection
				do: [ :each | 
					self bufferedStream
						nextPutAll: '    ';
						nextPutAll: each;
						lf ] ].
	self changedMethods isEmpty
		ifFalse: [ 
			self bufferedStream
				nextPutAll: ' -- Changed Methods --';
				lf.
			self changedMethods asSortedCollection
				do: [ :each | 
					self bufferedStream
						nextPutAll: '    ';
						nextPutAll: each;
						lf ] ].
	self changedMethodExtensions isEmpty
		ifFalse: [ 
			self bufferedStream
				nextPutAll: ' -- Changed Method Extensions --';
				lf.
			self changedMethodExtensions asSortedCollection
				do: [ :each | 
					self bufferedStream
						nextPutAll: '    ';
						nextPutAll: each;
						lf ] ].
	self deletedTraitMethods isEmpty
		ifFalse: [ 
			self bufferedStream
				nextPutAll: ' -- Deleted Trait Methods --';
				lf.
			self deletedTraitMethods asSortedCollection
				do: [ :each | 
					self bufferedStream
						nextPutAll: '    ';
						nextPutAll: each;
						lf ] ].
	self addedTraitMethods isEmpty
		ifFalse: [ 
			self bufferedStream
				nextPutAll: ' -- Added Trait Methods --';
				lf.
			self addedTraitMethods asSortedCollection
				do: [ :each | 
					self bufferedStream
						nextPutAll: '    ';
						nextPutAll: each;
						lf ] ].
	self changedTraitMethods isEmpty
		ifFalse: [ 
			self bufferedStream
				nextPutAll: ' -- Changed Trait Methods --';
				lf.
			self changedTraitMethods asSortedCollection
				do: [ :each | 
					self bufferedStream
						nextPutAll: '    ';
						nextPutAll: each;
						lf ] ].
	self bufferedStream nextPutAll: self reportFileFooter.

	self bufferedStream
		flush;
		close
]

{ #category : 'reporting' }
RwModificationReportWriterVisitor >> reportClassDefinitions: classDefinitionsInOrder [

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

{ #category : 'reporting' }
RwModificationReportWriterVisitor >> reportClassInitializations [
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
		stream nextPutAll: classDef name ; nextPutAll: ' initialize.'; lf 
  ].
  exclude ifFalse:[
	  stream nextPutAll: 'true'; lf;
	  nextPutAll: '%'; lf .
  ].
]

{ #category : 'reporting' }
RwModificationReportWriterVisitor >> reportExtensionMethodDefinitions: classDefinitionsInOrder [

	self reportMethodDefinitions: classDefinitionsInOrder labeled: 'Class extensions'
]

{ #category : 'accessing' }
RwModificationReportWriterVisitor >> reportFileFooter [
	^reportFileFooter
]

{ #category : 'accessing' }
RwModificationReportWriterVisitor >> reportFileFooter: object [
	reportFileFooter := object
]

{ #category : 'accessing' }
RwModificationReportWriterVisitor >> reportFileHeader [
	^reportFileHeader
]

{ #category : 'accessing' }
RwModificationReportWriterVisitor >> reportFileHeader: object [
	reportFileHeader := object
]

{ #category : 'accessing' }
RwModificationReportWriterVisitor >> reportFilename [
	^reportFilename
]

{ #category : 'accessing' }
RwModificationReportWriterVisitor >> reportFilename: object [
	reportFilename := object
]

{ #category : 'reporting' }
RwModificationReportWriterVisitor >> reportMethodDefinitions: classDefinitionsInOrder [

	self reportMethodDefinitions: classDefinitionsInOrder labeled: 'Class implementation'
]

{ #category : 'reporting' }
RwModificationReportWriterVisitor >> reportMethodDefinitions: classDefinitionsInOrder labeled: label [
	| stream |
	stream := self bufferedStream.
	classDefinitionsInOrder do: [:classDef | 
		| className |
		className := classDef name.
		(classDef classMethodDefinitions isEmpty not or: [ classDef instanceMethodDefinitions isEmpty not])
			ifTrue: [ 
				stream nextPutAll: '! '; nextPutAll: label ; nextPutAll: ' for ' ; nextPutAll: className printString; lf;
				lf ].
		classDef classMethodDefinitions isEmpty not
			ifTrue: [ 
				stream nextPutAll: '!		Class methods for ' ; nextPutAll: className printString; lf;
				lf ].
		(classDef classMethodDefinitions values sort: [:a :b | a selector <= b selector ])
			do: [:methodDef |
				methodDef selector == #initialize
					ifTrue: [ self classInitializationDefinitions add: classDef ].
				self _fileOutMethod: methodDef forClass: className isMeta: true on: stream ].
		classDef instanceMethodDefinitions isEmpty 
			ifFalse: [ 
				stream nextPutAll: '!		Instance methods for ' ; nextPutAll: className printString; lf;
				lf ].
		(classDef instanceMethodDefinitions values sort: [:a :b | a selector <= b selector ])
			do: [:methodDef |
				self _fileOutMethod: methodDef forClass: className isMeta: false on: stream ] ].
]

{ #category : 'reporting' }
RwModificationReportWriterVisitor >> reportTraitDefinitions: traitDefinitionsInOrder [
	| stream |
	stream := self bufferedStream.
	traitDefinitionsInOrder isEmpty not
		ifTrue: [ 
			stream
				nextPutAll: '! Trait Declarations';
				lf;
				nextPutAll: '! Generated file, do not Edit';
				lf;
				lf ].
	traitDefinitionsInOrder
		do: [ :traitDef | self _fileOutTraitDeclaration: traitDef on: stream ]
]

{ #category : 'reporting' }
RwModificationReportWriterVisitor >> reportTraitMethodDefinitions: traitDefinitionsInOrder [

	self reportTraitMethodDefinitions: traitDefinitionsInOrder labeled: 'Trait implementation'
]

{ #category : 'reporting' }
RwModificationReportWriterVisitor >> reportTraitMethodDefinitions: traitDefinitionsInOrder labeled: label [
	| stream |
	stream := self bufferedStream.
	traitDefinitionsInOrder do: [:traitDef | 
		| traitName |
		traitName := traitDef name.
		(traitDef classMethodDefinitions isEmpty not or: [ traitDef instanceMethodDefinitions isEmpty not])
			ifTrue: [ 
				stream nextPutAll: '! '; nextPutAll: label ; nextPutAll: ' for ' ; nextPutAll: traitName printString; lf;
				lf ].
		traitDef classMethodDefinitions isEmpty not
			ifTrue: [ 
				stream nextPutAll: '!		Class methods for ' ; nextPutAll: traitName printString; lf;
				lf ].
		(traitDef classMethodDefinitions values sort: [:a :b | a selector <= b selector ])
			do: [:methodDef |
				self _fileOutMethod: methodDef forTrait: traitName isMeta: true on: stream ].
		traitDef instanceMethodDefinitions isEmpty 
			ifFalse: [ 
				stream nextPutAll: '!		Instance methods for ' ; nextPutAll: traitName printString; lf;
				lf ].
		(traitDef instanceMethodDefinitions values sort: [:a :b | a selector <= b selector ])
			do: [:methodDef |
				self _fileOutMethod: methodDef forTrait: traitName isMeta: false on: stream ] ].
]

{ #category : 'reporting' }
RwModificationReportWriterVisitor >> reportTraitsForClasses: classDefinitionsInOrder [
	| stream |
	stream := self bufferedStream.
	classDefinitionsInOrder
		do: [ :classDef | 
			(classDef traits isEmpty and: [ classDef classTraits isEmpty ])
				ifFalse: [ 
					stream
						nextPutAll: '! ------------------- Traits for ';
						_fileOutAll: classDef name;
						lf.
					stream
						nextPutAll: 'expectvalue /Class';
						lf;
						nextPutAll: 'doit';
						lf.
					classDef traits isEmpty
						ifFalse: [ 
							stream
								_fileOutAll: classDef name;
								nextPutAll: ' addTrait: ' , classDef traits , '.';
								lf ].
					classDef classTraits isEmpty
						ifFalse: [ 
							stream
								_fileOutAll: classDef name;
								nextPutAll: ' addClassTrait: ' , classDef classTraits , '.';
								lf ].
					stream
						nextPut: $%;
						lf ] ]
]

{ #category : 'accessing' }
RwModificationReportWriterVisitor >> repositoryRootPath [

	^ repositoryRootPath
]

{ #category : 'accessing' }
RwModificationReportWriterVisitor >> repositoryRootPath: aStringOrFileReference [ 

	repositoryRootPath := aStringOrFileReference asFileReference.
	repositoryRootPath exists ifFalse: [ self error: 'The repository root path must exist: ', repositoryRootPath pathString printString ]
]

{ #category : 'accessing' }
RwModificationReportWriterVisitor >> topazFileReference [

	^ self repositoryRootPath / self topazFilename, self filenameExtension
]

{ #category : 'accessing' }
RwModificationReportWriterVisitor >> traitDefinitions [

	^ traitDefinitions ifNil: [ traitDefinitions := Dictionary new ]
]

{ #category : 'public' }
RwModificationReportWriterVisitor >> visit: aPackageSetModification [

	aPackageSetModification acceptVisitor: self.
	self report.
]

{ #category : 'visiting' }
RwModificationReportWriterVisitor >> visitClassModification: aClassModification [

	aClassModification isAddition
		ifTrue: [ ^ self addedClass: aClassModification ] .
	aClassModification isDeletion
		ifTrue: [ ^ self deletedClass: aClassModification ].
(aClassModification instanceMethodsModification isEmpty and: [ aClassModification classMethodsModification isEmpty ])
	ifTrue: [
		"no instance or class modifications, check to see if the only Property modification is for 'gs_SymbolDictionary' "
		| propertiesModification elementsModified |
		propertiesModification := aClassModification propertiesModification.
		elementsModified := propertiesModification elementsModified.
		(elementsModified size = 1 and: [ (elementsModified at: #'gs_SymbolDictionary' ifAbsent: []) notNil])
			ifTrue: [
				" #'gs_symbolDictionary' property is not written to disk, so we can skip this class"
				^ self ]].
	^ self changedClass: aClassModification
]

{ #category : 'visiting' }
RwModificationReportWriterVisitor >> visitExtensionMethodModification: aMethodExtensionModification [
	aMethodExtensionModification isAddition
		ifTrue: [ ^ self addedMethodExtension: aMethodExtensionModification ] .
	aMethodExtensionModification isDeletion
		ifTrue: [ ^ self deletedMethodExtension: aMethodExtensionModification ].
	^ self changedMethodExtension: aMethodExtensionModification
]

{ #category : 'visiting' }
RwModificationReportWriterVisitor >> visitExtensionMethodsModification: aMethodExtensionsModification [

	aMethodExtensionsModification elementsModified
		do: [ :each | each acceptVisitor: self ]
]

{ #category : 'visiting' }
RwModificationReportWriterVisitor >> visitMethodModification: aMethodModification [

	aMethodModification isAddition
		ifTrue: [ ^ self addedMethod: aMethodModification ] .
	aMethodModification isDeletion
		ifTrue: [ ^ self deletedMethod: aMethodModification ].
	^ self changedMethod: aMethodModification
]

{ #category : 'visiting' }
RwModificationReportWriterVisitor >> visitMethodsModification: aMethodsModification [

	aMethodsModification elementsModified
		do: [:each | each acceptVisitor: self]
]

{ #category : 'visiting' }
RwModificationReportWriterVisitor >> visitTraitMethodModification: aTraitMethodModification [
	aTraitMethodModification isAddition
		ifTrue: [ ^ self addedTraitMethod: aTraitMethodModification ].
	aTraitMethodModification isDeletion
		ifTrue: [ ^ self deletedTraitMethod: aTraitMethodModification ].
	^ self changedTraitMethod: aTraitMethodModification
]

{ #category : 'visiting' }
RwModificationReportWriterVisitor >> visitTraitMethodsModification: aTraitMethodsModification [

	aTraitMethodsModification elementsModified
		do: [:each | each acceptVisitor: self]
]
