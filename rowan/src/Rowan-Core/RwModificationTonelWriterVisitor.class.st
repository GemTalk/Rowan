Class {
	#name : 'RwModificationTonelWriterVisitor',
	#superclass : 'RwModificationCypressFamilyWriterVisitor',
	#category : 'Rowan-Core'
}

{ #category : 'accessing' }
RwModificationTonelWriterVisitor class >> classLabel [
	^ 'Class'
]

{ #category : 'class extension writing' }
RwModificationTonelWriterVisitor >> _classExtensionSourceFile [

	| filename |
	filename := classExtFileNameMap at:  self currentClassExtension name.
	^ self _packageSourceDir / (filename, '.extension.st')
]

{ #category : 'class writing' }
RwModificationTonelWriterVisitor >> _classSourceFile [

	| filename |
	filename := classDefFileNameMap at:  self currentClassDefinition name.
	^ self _packageSourceDir / (filename, '.class.st')
]

{ #category : 'class writing' }
RwModificationTonelWriterVisitor >> _fileNameFor: aClassDefinition [
	^ String streamContents: [ :stream | 
		stream 
			<< aClassDefinition name
			<< '.' << (self _typeOfClass: aClassDefinition)
			<< '.st'  ]
]

{ #category : 'class writing' }
RwModificationTonelWriterVisitor >> _hasCommentOf: aClassDefinition [ 

	| comment |
	comment := aClassDefinition comment.
	^ comment notNil and: [ comment ~= '' ]
]

{ #category : 'class writing' }
RwModificationTonelWriterVisitor >> _methodDefinitionOf: aMethodDefinition [
	^ self _toSTON: (self class orderedDictionaryClass new 
		at: #category put: aMethodDefinition protocol; 
		yourself)
]

{ #category : 'class writing' }
RwModificationTonelWriterVisitor >> _selectorIsComplete: keywords in: aString [
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
RwModificationTonelWriterVisitor >> _splitMethodSource: aMethodDefinition into: aBlock [
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
RwModificationTonelWriterVisitor >> _typeClassDefinitionOf: aClassDefinition [
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

	definition 		
		at: #category put: aClassDefinition category.
	
	^ self _toSTON: definition
]

{ #category : 'class writing' }
RwModificationTonelWriterVisitor >> _typeOfClass: aClassDefinition [
	^ self class classLabel asLowercase
]

{ #category : 'class writing' }
RwModificationTonelWriterVisitor >> _writeClassDefinition: aClassDefinition on: aStream [
	| nl |
	nl := self _newLine.
	
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
RwModificationTonelWriterVisitor >> _writeClassExtension: aClassExtension on: aStream [

	aStream << 'Extension '
				<< (self _toSTON: {(#'name' -> aClassExtension name)} asDictionary) << self _newLine.
]

{ #category : 'class writing' }
RwModificationTonelWriterVisitor >> _writeClassSideMethodDefinitions: aClassDefinition on: aStream [
	((aClassDefinition classMethodDefinitions values )
		sortWithBlock: [ :a :b | a selector _unicodeLessThan: b selector ])
		do: [ :each | 
			self _writeMethodDefinition: each classDefinition: aClassDefinition isMeta: true on: aStream ]
]

{ #category : 'class writing' }
RwModificationTonelWriterVisitor >> _writeInstanceSideMethodDefinitions: aClassDefinition on: aStream [
	((aClassDefinition instanceMethodDefinitions values )
		sortWithBlock: [ :a :b | a selector _unicodeLessThan: b selector ])
		do: [ :each | 
			self _writeMethodDefinition: each classDefinition: aClassDefinition isMeta: false on: aStream ]
]

{ #category : 'class writing' }
RwModificationTonelWriterVisitor >> _writeMethodDefinition: aMethodDefinition  classDefinition: aClassDefinition isMeta: isMeta on: aStream [
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
RwModificationTonelWriterVisitor >> addedClassExtension: aClassExtensionModification [

	currentClassExtension := aClassExtensionModification after.

	self processClassExtension: aClassExtensionModification
]

{ #category : 'actions' }
RwModificationTonelWriterVisitor >> addedPackage: aPackageModification [

	currentPackageDefinition := aPackageModification after.

	"create package directory and package.st file"
	self _packageSourceDir ensureCreateDirectory.
	(self _packageSourceDir /  'package.st')
		writeStreamDo: [:aStream |
		aStream 
			<< 'Package ' 
			<< (self _toSTON: { #name ->  currentPackageDefinition name } asDictionary) 
			<< self  _newLine ].

	self processPackage: aPackageModification
]

{ #category : 'class writing' }
RwModificationTonelWriterVisitor >> processClass: aClassModification [

	"write out the class definition"

	self _classSourceFile
		writeStreamDo: [:aStream |
			self _writeClassDefinition: currentClassDefinition on: aStream.
			self _writeClassSideMethodDefinitions: currentClassDefinition on: aStream.
			self _writeInstanceSideMethodDefinitions: currentClassDefinition on: aStream ]
]

{ #category : 'class extension writing' }
RwModificationTonelWriterVisitor >> processClassExtension: aClassExtensionModification [

	"write out the class definition"

	self _classExtensionSourceFile
		writeStreamDo: [:aStream |
			self _writeClassExtension: currentClassExtension on: aStream.
			self _writeClassSideMethodDefinitions: currentClassExtension on: aStream.
			self _writeInstanceSideMethodDefinitions: currentClassExtension on: aStream ]
]

{ #category : 'project writing' }
RwModificationTonelWriterVisitor >> processProject: aProjectModification [
	"confirm that the project source is written in Tonel format"

	| format |
	packageDefFileNameMap := self _createFileNameMapForClassesOrPackages:  aProjectModification after packages.
	(format := self _repositoryFormatFor: aProjectModification) = 'tonel' ifFalse: [ self error: 'expected tonel format repository, instead format is ', format printString ].
	super processProject: aProjectModification.
]
