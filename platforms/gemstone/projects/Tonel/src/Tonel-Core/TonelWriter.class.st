Class {
	#name : 'TonelWriter',
	#superclass : 'Object',
	#instVars : [
		'packageWriter'
	],
	#category : 'Tonel-Core'
}

{ #category : 'accessing' }
TonelWriter class >> classLabel [
	^ 'Class'

]

{ #category : 'accessing' }
TonelWriter class >> extensionLabel [
	^ 'Extension'

]

{ #category : 'instance creation' }
TonelWriter class >> on: aPackageWriter [

	^ self new
		packageWriter: aPackageWriter;
		yourself

]

{ #category : 'accessing' }
TonelWriter class >> traitLabel [
	^ 'Trait'

]

{ #category : 'private' }
TonelWriter >> classNameFor: aMethodDefinition parent: aClassDefinition [
	aClassDefinition ifNil: [ ^ aMethodDefinition fullClassName ].
	^ aMethodDefinition classIsMeta
		ifFalse: [ aMethodDefinition className ]
		ifTrue: [ 
			aClassDefinition isTraitDefinition
				ifFalse: [aMethodDefinition className, ' class']
				ifTrue: [aMethodDefinition className, ' classSide'] ]

]

{ #category : 'private definitions' }
TonelWriter >> commentOf: aClassDefinition [
	^ (aClassDefinition comment 
		copyReplaceAll: '"' 
		with: '""')
		withLineEndings: self newLine

]

{ #category : 'accessing' }
TonelWriter >> definitions [

   ^self packageWriter definitions

]

{ #category : 'private' }
TonelWriter >> fileNameFor: aClassDefinition [
	^ String streamContents: [ :stream | 
		stream 
			<< aClassDefinition className
			<< '.' << (self typeOf: aClassDefinition) asLowercase
			<< '.st'  ]

]

{ #category : 'private testing' }
TonelWriter >> isClass: aClassDefinition [
	^ self packageWriter isClass: aClassDefinition

]

{ #category : 'private testing' }
TonelWriter >> isTrait: aClassDefinition [
	^ self packageWriter isTrait: aClassDefinition

]

{ #category : 'private definitions' }
TonelWriter >> methodDefinitionOf: aMethodDefinition [
	^ self toSTON: (self class orderedDictionaryClass new 
		at: #category put: aMethodDefinition category asSymbol; 
		yourself)	
	

]

{ #category : 'private' }
TonelWriter >> newLine [
	 ^ self class lineEnding

]

{ #category : 'accessing' }
TonelWriter >> packageWriter [
  ^ packageWriter

]

{ #category : 'accessing' }
TonelWriter >> packageWriter: anObject [

   packageWriter := anObject

]

{ #category : 'private' }
TonelWriter >> selectorIsComplete: keywords in: aString [
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

{ #category : 'private' }
TonelWriter >> skipComment: aStream [
	"I assume I'm on top of the begining of a comment"
	aStream skip: 1.
	[ aStream atEnd not 
		and: [ aStream next ~= $" or: [ aStream peek = $" ] ] ]
	whileTrue.	

]

{ #category : 'private' }
TonelWriter >> skipSeparators: aStream [
	[ aStream peek isSeparator ]
	whileTrue: [ aStream skip: 1 ]. 

]

{ #category : 'private' }
TonelWriter >> splitMethodSource: aMethodDefinition into: aBlock [
	| keywords source declaration |
	
	keywords := aMethodDefinition selector asSymbol keywords.
	source := aMethodDefinition source readStream.
	"Skip spaces"
	(source peek isSeparator) ifTrue: [ self skipSeparators: source ].
	"Skip comments"
	(source peek = $") ifTrue: [ self skipComment: source ]. 
	"Parse declaration"
	declaration := String new writeStreamPortable.
	[ (self selectorIsComplete: keywords in: declaration originalContents) not 
		or: [ ':+-/\*~<>=@,%|&?!' includes: declaration contents trimRight last ] ]
	whileTrue: [ 
		"get separators"
		[ source atEnd not and: [ source peek isSeparator ] ]
			whileTrue: [ declaration nextPut: source next ].
		"take next word"
		[ source atEnd not and: [ source peek isSeparator not ] ]
			whileTrue: [ declaration nextPut: source next ] ].
	aBlock 
		value: (declaration contents trimLeft withLineEndings: self newLine)
		value: (source upToEnd withLineEndings: self newLine)

]

{ #category : 'private' }
TonelWriter >> toSTON: anObject [
	^ (String streamContents: [ :stream | 
		(TonelSTONWriter on: stream) nextPut: anObject ])
		withLineEndings: self newLine

]

{ #category : 'private definitions' }
TonelWriter >> typeClassDefinitionOf: aClassDefinition [
	| definition |
	
	definition := self class orderedDictionaryClass new 
		at: #name put: aClassDefinition className asSymbol; 
		at: #superclass put: aClassDefinition superclassName asSymbol;
		yourself.

	aClassDefinition type = #normal ifFalse: [ 
		definition at: #type put: aClassDefinition type ].
	
	aClassDefinition hasTraitComposition ifTrue: [ 
		definition at: #traits put: aClassDefinition traitCompositionString ].
	
	aClassDefinition hasClassTraitComposition ifTrue: [ 
		definition at: #classTraits put: aClassDefinition classTraitCompositionString ].
	
	(aClassDefinition instVarNames)
		ifNotEmpty: [ :vars | definition at: #instVars put: vars asArray ].

	(aClassDefinition classVarNames)
		ifNotEmpty: [ :vars | definition at: #classVars put: vars asArray ].
		
	((aClassDefinition poolDictionaries) collect: [:each | each asString])
		ifNotEmpty: [ :vars | definition at: #pools put: vars asArray ].
		
	(aClassDefinition classInstVarNames)
		ifNotEmpty: [ :vars | definition at: #classInstVars put: vars asArray ].

	(aClassDefinition gs_constraints)
		ifNotEmpty: [:gs_constraints | definition at: #'gs_constraints' put: gs_constraints asArray ].

	(aClassDefinition gs_options)
		ifNotEmpty: [:gs_options | definition at: #'gs_options' put: gs_options asArray ].

	definition 		
		at: #category put: aClassDefinition category asSymbol.
	
	^ self toSTON: definition

]

{ #category : 'private definitions' }
TonelWriter >> typeDefinitionOf: aClassDefinition [
	(self isTrait: aClassDefinition) 
		ifTrue: [ ^ self typeTraitDefinitionOf: aClassDefinition ].
	^ self typeClassDefinitionOf: aClassDefinition

]

{ #category : 'private' }
TonelWriter >> typeOf: aClassDefinition [
	(self isClass: aClassDefinition) ifTrue: [ ^ self class classLabel ].
	(self isTrait: aClassDefinition) ifTrue: [ ^ self class traitLabel ].

	TonelShouldIgnore signal

]

{ #category : 'writing' }
TonelWriter >> writeClass: aClassDefinition [
	[ 
		self packageWriter 
			writePackageFileNamed: (self fileNameFor: aClassDefinition) 
			do:  [ :aStream | 
				self writeClassDefinition: aClassDefinition on: aStream.
				self writeClassSideMethodDefinitions: aClassDefinition on: aStream.
				self writeInstanceSideMethodDefinitions: aClassDefinition on: aStream ] ]
	on: TonelShouldIgnore
	do: [ :e | self logCr: 'ignoring: ', aClassDefinition asString ]

]

{ #category : 'private writing' }
TonelWriter >> writeClassDefinition: aClassDefinition on: aStream [
	| nl |
	nl := self newLine.
	
	aClassDefinition hasComment 
		ifTrue: [ 
			aStream 
				<< '"' << nl
				<< (self commentOf: aClassDefinition) << nl
				<< '"' << nl ].
	aStream
		<< (self typeOf: aClassDefinition) 
		<< ' ' << (self typeDefinitionOf: aClassDefinition ) << nl

]

{ #category : 'private writing' }
TonelWriter >> writeClassSideMethodDefinitions: aClassDefinition on: aStream [
	((self definitions 
		select: [ :each | 
			each isMethodDefinition 
			and: [ each className = aClassDefinition className
			and: [ each classIsMeta ] ] ])
		sortWithBlock: [ :a :b | a selector _unicodeLessThan: b selector ])
		do: [ :each | 
			self writeMethodDefinition: each parent: aClassDefinition on: aStream ]

]

{ #category : 'writing' }
TonelWriter >> writeExtensionMethods: methods className: className [

	| nl |
	nl := self newLine.
	self packageWriter
		writePackageFileNamed: className , '.extension.st'
		do: [ :s | 
			s << 'Extension '
				<< (self toSTON: {(#'name' -> className asSymbol)} asDictionary) << nl.
			((methods select: [ :m | m classIsMeta not ])
				sortWithBlock: [ :a :b | a selector  _unicodeLessThan: b selector ])
				do: [ :each | self writeMethodDefinition: each on: s ].
			((methods select: [ :m | m classIsMeta ])
				sortWithBlock: [ :a :b | a selector  _unicodeLessThan: b selector ])
				do: [ :each | self writeMethodDefinition: each on: s ] ]

]

{ #category : 'private writing' }
TonelWriter >> writeInstanceSideMethodDefinitions: aClassDefinition on: aStream [
	((self definitions 
		select: [ :each | 
			each isMethodDefinition 
			and: [ each className = aClassDefinition className
			and: [ each classIsMeta not ] ] ])
		sortWithBlock: [ :a :b | a selector _unicodeLessThan: b selector ])
		do: [ :each | 
			self writeMethodDefinition: each parent: aClassDefinition on: aStream ]

	
	

]

{ #category : 'private writing' }
TonelWriter >> writeMethodDefinition: aMethodDefinition on: aStream [
	^ self 
		writeMethodDefinition: aMethodDefinition 
		parent: nil 
		on: aStream

]

{ #category : 'private writing' }
TonelWriter >> writeMethodDefinition: aMethodDefinition parent: aClassDefinition on: aStream [
	| nl |
	
	nl := self newLine.
	self 
		splitMethodSource: aMethodDefinition 
		into: [ :methodDeclaration :methodBody | | fullClassName |
			fullClassName := self classNameFor: aMethodDefinition parent: aClassDefinition.
			aStream 
				<< nl 
				<< (self methodDefinitionOf: aMethodDefinition) << nl 
				<< fullClassName << ' >> ' << methodDeclaration 
				<< ' [' << methodBody << nl << ']' << nl ]

]

{ #category : 'writing' }
TonelWriter >> writePackage: packageName [
	self packageWriter 
		writePackageFileNamed: 'package.st'
		do:  [ :aStream | self writePackage: packageName on: aStream ]

]

{ #category : 'writing' }
TonelWriter >> writePackage: packageName on: aStream [

	aStream 
		<< 'Package ' 
		<< (self toSTON: { #name ->  packageName asSymbol } asDictionary) 
		<< self  newLine

]
