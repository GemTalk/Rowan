Class {
	#name : 'RwModificationTonelWriterVisitorV2',
	#superclass : 'RwModificationCypressFamilyWriterVisitorV2',
	#category : 'Rowan-Core'
}

{ #category : 'accessing' }
RwModificationTonelWriterVisitorV2 class >> classLabel [
	^ 'Class'
]

{ #category : 'class extension writing' }
RwModificationTonelWriterVisitorV2 >> _classExtensionSourceFile [

	| filename |
	filename := classExtFileNameMap 
		at:  self currentClassExtension name
		ifAbsent: [
			"in the case of class removal, need to use the before class def map"
			classExtBeforeFileNameMap at: self currentClassExtension name ].
	^ self _packageSourceDir / (filename, '.extension.st')
]

{ #category : 'class writing' }
RwModificationTonelWriterVisitorV2 >> _classSourceFile [

	| filename |
	filename := classDefFileNameMap 
		at:  self currentClassDefinition name
		ifAbsent: [
			"in the case of class removal, need to use the before class def map"
			classDefBeforeFileNameMap at: self currentClassDefinition name ].
	^ self _packageSourceDir / (filename, '.class.st')
]

{ #category : 'class writing' }
RwModificationTonelWriterVisitorV2 >> _commentOf: aClassDefinition [
	^ (aClassDefinition comment 
		copyReplaceAll: '"' 
		with: '""')
		withLineEndings: self _newLine
]

{ #category : 'class writing' }
RwModificationTonelWriterVisitorV2 >> _hasCommentOf: aClassDefinition [ 

	| comment |
	comment := aClassDefinition comment.
	^ comment notNil and: [ comment ~= '' ]
]

{ #category : 'class writing' }
RwModificationTonelWriterVisitorV2 >> _hasShebangOf: aClassDefinition [ 

	^  aClassDefinition shebang notNil
]

{ #category : 'class writing' }
RwModificationTonelWriterVisitorV2 >> _methodDefinitionOf: aMethodDefinition [
	| excludedMethodProperties methodProperties exportedProperties |
	excludedMethodProperties := #('_gsFileOffset' '_gsFileName' 'category' 'protocol' 'selector').
	exportedProperties := self class orderedDictionaryClass new
		at: #'category' put: aMethodDefinition protocol;
		yourself.
	methodProperties := aMethodDefinition properties.
	(methodProperties keys
		reject: [ :each | excludedMethodProperties includes: each ]) asArray sort
		do: [ :key | exportedProperties at: key asSymbol put: (methodProperties at: key) ].
	^ self _toSTON: exportedProperties
]

{ #category : 'class writing' }
RwModificationTonelWriterVisitorV2 >> _selectorIsComplete: keywords in: aString [
	| start |
	
	start := 1.
	keywords do: [ :each | | index | 
		index := aString 
			findString: each 
			startingAt: start 
			caseSensitive: true.
		index = 0 ifTrue: [ ^ false ].
		start := index + each size ].
	^ true
]

{ #category : 'class writing' }
RwModificationTonelWriterVisitorV2 >> _shebangOf: aClassDefinition [

	^ aClassDefinition shebang
]

{ #category : 'class writing' }
RwModificationTonelWriterVisitorV2 >> _splitMethodSource: aMethodDefinition into: aBlock [
	| keywords source declaration |
	
	keywords := aMethodDefinition selector asSymbol keywords.
	source := aMethodDefinition source readStream.
	"Skip spaces"
	(source peek isSeparator) ifTrue: [ self skipSeparators: source ].
	"Skip comments"
	(source peek = $") ifTrue: [ self skipComment: source ]. 
	"Parse declaration"
	declaration := String new writeStreamPortable.
	[ (self _selectorIsComplete: keywords in: declaration originalContents) not 
		or: [ ':+-/\*~<>=@,%|&?!' includes: declaration contents trimRight last ] ]
	whileTrue: [ 
		"get separators"
		[ source atEnd not and: [ source peek isSeparator ] ]
			whileTrue: [ declaration nextPut: source next ].
		"take next word"
		[ source atEnd not and: [ source peek isSeparator not ] ]
			whileTrue: [ declaration nextPut: source next ] ].
	aBlock 
		value: (declaration contents trimLeft withLineEndings: self _newLine)
		value: (source upToEnd withLineEndings: self _newLine)
]

{ #category : 'class writing' }
RwModificationTonelWriterVisitorV2 >> _typeClassDefinitionOf: aClassDefinition [
	| definition |
	
	definition := self class orderedDictionaryClass new 
		at: #name put: aClassDefinition name; 
		at: #superclass put: aClassDefinition superclassName;
		yourself.

	aClassDefinition classType = 'normal' ifFalse: [ 
		definition at: #type put: aClassDefinition classType ].
	
	(aClassDefinition instVarNames)
		ifNotEmpty: [ :vars | definition at: #instVars put: vars asArray ].

	(aClassDefinition classVarNames)
		ifNotEmpty: [ :vars | definition at: #classVars put: vars asArray ].
		
	((aClassDefinition poolDictionaryNames) collect: [:each | each asString])
		ifNotEmpty: [ :vars | definition at: #pools put: vars asArray ].
		
	(aClassDefinition classInstVarNames)
		ifNotEmpty: [ :vars | definition at: #classInstVars put: vars asArray ].

	(aClassDefinition gs_constraints)
		ifNotEmpty: [:gs_constraints | definition at: #'gs_constraints' put: gs_constraints asArray ].

	(aClassDefinition gs_options)
		ifNotEmpty: [:gs_options | definition at: #'gs_options' put: gs_options asArray ].

	(aClassDefinition gs_reservedOop)
		ifNotEmpty: [:gs_reservedOop | definition at: #'gs_reservedoop' put: gs_reservedOop asString ].

	definition 		
		at: #category put: aClassDefinition category.
	
	^ self _toSTON: definition
]

{ #category : 'class writing' }
RwModificationTonelWriterVisitorV2 >> _typeOfClass: aClassDefinition [
	^ self class tonelClassLabel
]

{ #category : 'class writing' }
RwModificationTonelWriterVisitorV2 >> _writeClassDefinition: aClassDefinition on: aStream [
	| nl |
	nl := self _newLine.
	
	(self _hasShebangOf: aClassDefinition)
		ifTrue: [ aStream << '#!' << (self _shebangOf: aClassDefinition) << nl ].
	(self _hasCommentOf: aClassDefinition) 
		ifTrue: [ 
			aStream 
				<< '"' << nl
				<< (self _commentOf: aClassDefinition) << nl
				<< '"' << nl ].
	aStream
		<< (self _typeOfClass: aClassDefinition) 
		<< ' ' << (self _typeClassDefinitionOf: aClassDefinition ) << nl
]

{ #category : 'class extension writing' }
RwModificationTonelWriterVisitorV2 >> _writeClassExtension: aClassExtension on: aStream [

	aStream << 'Extension '
				<< (self _toSTON: {(#'name' -> aClassExtension name)} asDictionary) << self _newLine.
]

{ #category : 'class writing' }
RwModificationTonelWriterVisitorV2 >> _writeClassSideMethodDefinitions: aClassDefinition on: aStream [
	((aClassDefinition classMethodDefinitions values )
		sortWithBlock: [ :a :b | a selector _unicodeLessThan: b selector ])
		do: [ :each | 
			self _writeMethodDefinition: each classDefinition: aClassDefinition isMeta: true on: aStream ]
]

{ #category : 'class writing' }
RwModificationTonelWriterVisitorV2 >> _writeInstanceSideMethodDefinitions: aClassDefinition on: aStream [
	((aClassDefinition instanceMethodDefinitions values )
		sortWithBlock: [ :a :b | a selector _unicodeLessThan: b selector ])
		do: [ :each | 
			self _writeMethodDefinition: each classDefinition: aClassDefinition isMeta: false on: aStream ]
]

{ #category : 'class writing' }
RwModificationTonelWriterVisitorV2 >> _writeMethodDefinition: aMethodDefinition  classDefinition: aClassDefinition isMeta: isMeta on: aStream [
	| nl |
	
	nl := self _newLine.
	self 
		_splitMethodSource: aMethodDefinition 
		into: [ :methodDeclaration :methodBody | | fullClassName |
			fullClassName :=aClassDefinition name  , (isMeta ifTrue: [' class'] ifFalse: ['']).
			aStream 
				<< nl 
				<< (self _methodDefinitionOf: aMethodDefinition) << nl 
				<< fullClassName << ' >> ' << methodDeclaration 
				<< ' [' << methodBody << nl << ']' << nl ]
]

{ #category : 'actions' }
RwModificationTonelWriterVisitorV2 >> addedClassExtension: aClassExtensionModification [

	currentClassExtension := aClassExtensionModification after.

	self processClassExtension: aClassExtensionModification
]

{ #category : 'actions' }
RwModificationTonelWriterVisitorV2 >> addedPackage: aPackageModification [
	| packageProperties exportedPackageProperties |
	currentPackageDefinition := aPackageModification after.

	packageProperties := currentPackageDefinition properties.
	exportedPackageProperties := self class orderedDictionaryClass new.
	exportedPackageProperties at: #'name' put: (packageProperties at: 'name').
	(packageProperties at: 'comment' ifAbsent: [  ])
		ifNotNil: [ :comment | exportedPackageProperties at: #'comment' put: comment ].
	(packageProperties keys
		reject: [ :key | key = 'name' or: [ key = 'gs_SymbolDictionary' ] ]) asArray
		sort
		do: [ :key | exportedPackageProperties at: key asSymbol put: (packageProperties at: key) ].

	self _packageSourceDir ensureCreateDirectory.
	self _packageSourceDir / 'package.st'
		writeStreamDo: [ :aStream | 
			aStream << 'Package ' << (self _toSTON: exportedPackageProperties)
				<< self _newLine ].

	self processPackage: aPackageModification
]

{ #category : 'accessing' }
RwModificationTonelWriterVisitorV2 >> defaultPackageFormat [

	^ 'tonel'
]

{ #category : 'actions' }
RwModificationTonelWriterVisitorV2 >> deletedClass: aClassModification [

	currentClassDefinition := aClassModification before.

	self _classSourceFile ensureDelete
]

{ #category : 'actions' }
RwModificationTonelWriterVisitorV2 >> deletedClassExtension: aClassExtensionModification [

	currentClassExtension := aClassExtensionModification before.

	self _classExtensionSourceFile ensureDelete
]

{ #category : 'class writing' }
RwModificationTonelWriterVisitorV2 >> processClass: aClassModification [

	"write out the class definition"

	self _classSourceFile
		writeStreamDo: [:aStream |
			self _writeClassDefinition: currentClassDefinition on: aStream.
			self _writeClassSideMethodDefinitions: currentClassDefinition on: aStream.
			self _writeInstanceSideMethodDefinitions: currentClassDefinition on: aStream ]
]

{ #category : 'class extension writing' }
RwModificationTonelWriterVisitorV2 >> processClassExtension: aClassExtensionModification [

	"write out the class definition"

	(currentClassExtension instanceMethodDefinitions isEmpty and: [ currentClassExtension classMethodDefinitions isEmpty ])
		ifTrue: [ self _classExtensionSourceFile ensureDelete ]
		ifFalse: [ 
			self _classExtensionSourceFile
				writeStreamDo: [:aStream |
					self _writeClassExtension: currentClassExtension on: aStream.
					self _writeClassSideMethodDefinitions: currentClassExtension on: aStream.
					self _writeInstanceSideMethodDefinitions: currentClassExtension on: aStream ] ]
]

{ #category : 'project writing' }
RwModificationTonelWriterVisitorV2 >> processProject: aProjectModification [
	"confirm that the project source is written in Tonel format"

	| format propertiesFile |
	packageDefFileNameMap := self _createFileNameMapForClassesOrPackages:  aProjectModification after packages.
	packageDefBeforeFileNameMap := self _createFileNameMapForClassesOrPackages:  aProjectModification before packages.

	(format := self _repositoryFormatFor: self packagesRoot) = 'tonel' ifFalse: [ self error: 'expected tonel format repository, instead format is ', format printString ].

	propertiesFile := self packagesRoot /  'properties.st'.
	propertiesFile exists
		ifFalse: [
			propertiesFile writeStreamDo: [ :fileStream | 
				fileStream 
					nextPutAll: '{ #format : ' , format printString , '}';
					lf;
					nextPutAll: '{ #convention : ' , currentProjectDefinition packageConvention printString , '}';
					yourself ] ].

	super processProject: aProjectModification.
]

{ #category : 'private' }
RwModificationTonelWriterVisitorV2 >> skipComment: aStream [
  "I assume I'm on top of the begining of a comment"
  aStream skip: 1.
  [ aStream atEnd not 
    and: [ aStream next ~= $" or: [ aStream peek = $" ] ] ]
  whileTrue.  
]

{ #category : 'private' }
RwModificationTonelWriterVisitorV2 >> skipSeparators: aStream [
  [ aStream peek isSeparator ]
    whileTrue: [ aStream skip: 1 ]. 
]
