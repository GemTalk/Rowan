Class {
	#name : 'RwTonelParserTest',
	#superclass : 'TestCase',
	#category : 'Rowan-Tests'
}

{ #category : 'asserting' }
RwTonelParserTest >> assertParse: aString rule: rule equals: result [
	| parsed |
	parsed := self parse: aString rule: rule.
	(rule == #'methodDef' or: [ rule == #'typeDef' ])
		ifTrue: [ self assert: (parsed compareAgainstBase: result) isEmpty ]
		ifFalse: [ self assert: parsed equals: result ]
]

{ #category : 'private' }
RwTonelParserTest >> newClassDefinitionForClassNamed: nameString superclassName: superclassString category: categoryString instVarNames: ivarArray classVarNames: cvarArray poolDictionaryNames: poolArray classInstVarNames: civarArray type: type comment: commentString [
	^ RwClassDefinition
		newForClassNamed: nameString
		super: superclassString
		instvars: ivarArray
		classinstvars: civarArray
		classvars: cvarArray
		category: categoryString
		comment: commentString
		pools: poolArray
		type: type
]

{ #category : 'private' }
RwTonelParserTest >> newClassDefinitionFrom: anArray [
	| metadata classDef |
	metadata := anArray sixth.
	classDef := RwClassDefinition
		newForClassNamed: (metadata at: #'name') asString
		super:
			(metadata at: #'superclass' ifAbsent: [ metadata at: #'super' ]) asString
		instvars: (metadata at: #'instvars' ifAbsent: [ #() ])
		classinstvars: (metadata at: #'classinstvars' ifAbsent: [ #() ])
		classvars: (metadata at: #'classvars' ifAbsent: [ #() ])
		category: (metadata at: #'category' ifAbsent: [  ])
		comment: anArray second
		pools: (metadata at: #'pools' ifAbsent: [ #() ])
		type: (metadata at: #'type' ifAbsent: [ #'normal' ]) asSymbol.
	^ classDef
		gs_options: (metadata at: #'gs_options' ifAbsent: [ #() ]);
		gs_reservedOop: (metadata at: #'gs_reservedoop' ifAbsent: [ '' ]);
		gs_constraints: (metadata at: #'gs_constraints' ifAbsent: [ #() ]);
		shebang: (metadata at: #'shebang' ifAbsent: [  ]);
		yourself
]

{ #category : 'private' }
RwTonelParserTest >> newMethodDefinitionForClassNamed: className classIsMeta: meta selector: selector category: category source: source [
	^ RwMethodDefinition newForSelector: selector protocol: category source: source
]

{ #category : 'private' }
RwTonelParserTest >> newTraitDefinitionFrom: anArray [
	| traitName metadata instvars packageName category |
	metadata := anArray sixth.
	traitName := metadata at: #'name'.
	instvars := #().	"for now"
	category := metadata at: #'category' ifAbsent: [  ].
	category
		ifNil: [ 
			packageName := metadata
				at: #'packageName'
				ifAbsent: [ 
					self
						error:
							'One of package name or category must be specified for trait '
								, traitName printString ] ]
		ifNotNil: [ packageName := metadata at: #'packageName' ifAbsent: [ category ] ].
	^ RwTraitDefinition
		newForTraitNamed: traitName
		instanceVariableNames: instvars
		category: category
		packageName: packageName
]

{ #category : 'private' }
RwTonelParserTest >> newTypeDefinitionFrom: anArray [
	| typeString |
	typeString := anArray fourth.
	typeString = RwAbstractReaderWriterVisitor tonelClassLabel
		ifTrue: [ ^ self newClassDefinitionFrom: anArray ].
	typeString = RwAbstractReaderWriterVisitor tonelExtensionLabel
		ifTrue: [ ^ self newClassExtensionDefinitionFrom: anArray ].
	typeString = RwAbstractReaderWriterVisitor tonelTraitLabel
		ifTrue: [ ^ self newTraitDefinitionFrom: anArray ].
	^ self error: 'unexpected typeClass: ' , typeString
]

{ #category : 'private' }
RwTonelParserTest >> parse: aString rule: rule [
  | parser |
  parser := RwTonelParser onString: aString forReader: self.
  ^ parser perform: rule

]

{ #category : 'asserting' }
RwTonelParserTest >> shouldParse: aString rule: rule raise: error [
  | parser |
  parser := RwTonelParser onString: aString  forReader: self.
  self should: [ parser perform: rule ] raise: error

]

{ #category : 'tests' }
RwTonelParserTest >> testComment [
	self 
		shouldParse: 'this "should" be an error'
		rule: #comment
		raise: RwTonelParseError.
	
	self  
		assertParse: '"this is a comment"'
		rule: #comment
		equals: 'this is a comment'.
	
	self  
		assertParse: '"""this"" is a comment with ""nested"" colons ""also at the end"""'
		rule: #comment
		equals: '"this" is a comment with "nested" colons "also at the end"'	
		
		

]

{ #category : 'tests' }
RwTonelParserTest >> testExtractSelector [
	| parser |
	
	parser := RwTonelParser new.
	
	self assert: (parser extractSelector: 'unary') equals: #unary.
	self assert: (parser extractSelector: '+ something') equals: #+.
	self assert: (parser extractSelector: '==> other') equals: #==>.
	self 
		assert: (parser extractSelector: 'some: arg1 keyword: arg2 selector: arg3') 
		equals: #some:keyword:selector:.
	self 
		assert: (parser extractSelector: 'some: 	arg1 keyword:arg2 selector: arg3') 
		equals: #some:keyword:selector:.
	self 
		assert: (parser extractSelector: 'some: arg1 
keyword: arg2 
selector: arg3') 
		equals: #some:keyword:selector:.

]

{ #category : 'tests' }
RwTonelParserTest >> testMetadata [
	self 
		assertParse: '{ #vars: [ #a, #b ] }' 
		rule: #metadata 
		equals: { #vars -> #(a b) } asDictionary.
	
	self 
		assertParse: '{ 
	#vars: [ #a, #b ],
	#uses: { #someNested: 42 } 
	}' 
		rule: #metadata 
		equals: { 
			#vars -> #(a b). 
			#uses -> { #someNested -> 42 } asDictionary
		} asDictionary

]

{ #category : 'tests' }
RwTonelParserTest >> testMethod [
	self 
		assertParse: 'Object>>name' 
		rule: #method
		equals: #(('Object' nil) 'name').
		
	self 
		assertParse: 'Object >> name: aString' 
		rule: #method
		equals: #(('Object' nil) 'name: aString').
		
	self 
		assertParse: 'Object >> name: aString [ I do not care ]' 
		rule: #method
		equals: #(('Object' nil) 'name: aString').
		
	self 
		assertParse: 'Object class >> name: aString' 
		rule: #method
		equals: #(('Object' 'class') 'name: aString').

]

{ #category : 'tests' }
RwTonelParserTest >> testMethodBody [
	self 
		assertParse: '[ method body... I will ignore what is inside ]'
		rule: #methodBody
		equals: ' method body... I will ignore what is inside'.

	self 
		assertParse: '[
method 
	[body... [I 
		will ignore] 
what] is inside
]'
		rule: #methodBody
		equals: '
method 
	[body... [I 
		will ignore] 
what] is inside'.
		
	self 
		assertParse: '[ method body with "''", ''"'', "[", '']'' ]'
		rule: #methodBody
		equals: ' method body with "''", ''"'', "[", '']'''.
	

]

{ #category : 'tests' }
RwTonelParserTest >> testMethodDef [
	[ 
	self
		assertParse:
			'{ #category: ''accessing'' }
Object>>name [
	^ self printString
]'
		rule: #'methodDef'
		equals:
			(self
				newMethodDefinitionForClassNamed: #'Object'
				classIsMeta: false
				selector: #'name'
				category: 'accessing'
				source:
					'name
	^ self printString').

	self
		assertParse:
			'{ #category: ''changed'' }
Object class>>name [ 
	^ self printString
]'
		rule: #'methodDef'
		equals:
			(self
				newMethodDefinitionForClassNamed: #'Object'
				classIsMeta: true
				selector: #'name'
				category: 'changed'
				source:
					'name 
	^ self printString').

	self
		assertParse:
			'{ #category: ''accessing'' }
TClass classSide >> template: aSystemCategoryName [ 
	"I really do not care"
]'
		rule: #'methodDef'
		equals:
			(self
				newMethodDefinitionForClassNamed: #'TClass'
				classIsMeta: true
				selector: #'template:'
				category: 'accessing'
				source:
					'template: aSystemCategoryName 
	"I really do not care"') ]
		on: RwTonelParseRequireMethodCategoryNotification
		do: [ :ex | ex resume: '' ]
]

{ #category : 'tests' }
RwTonelParserTest >> testMethodDef_627_01 [
	self
		shouldParse:
			'
{ #category: ''accessing'' }
:Object>>name [
	^ self printString
]'
		rule: #'methodDef'
		raise: RwTonelParseError
]

{ #category : 'tests' }
RwTonelParserTest >> testMethodDef_627_02 [
	"Missing method category... error"

	self
		shouldParse:
			'( #category: ''accessing'' }
Object>>name [
	^ self printString
]'
		rule: #'methodDef'
		raise: Error
]

{ #category : 'tests' }
RwTonelParserTest >> testMethodDef_627_03 [
	self
		shouldParse:
			'
.
{ #category: ''accessing'' }
Object>>name [
	^ self printString
]'
		rule: #'methodDef'
		raise: Error
]

{ #category : 'tests' }
RwTonelParserTest >> testMethodDef_627_04 [
	self
		shouldParse:
			'
!
{ #category: ''accessing'' }
Object>>name [
	^ self printString
]'
		rule: #'methodDef'
		raise: Error
]

{ #category : 'tests' }
RwTonelParserTest >> testMethodDefList [
	| parsed |
	[ 
	parsed := self
		parse:
			'{ #category: ''accessing'' }
Object class>>new [
	^ self basicNew initialize
]

{ #category: ''accessing'' }
Object>>name [
	^ self printString
]

{ #category: ''printing'' }
Object>>printOn: aStream [
	"Append to the argument, aStream, a sequence of characters that  
	identifies the receiver."

	| title |
	title := self class name.
	aStream
		nextPutAll: (title first isVowel ifTrue: [''an ''] ifFalse: [''a '']);
		nextPutAll: title

]'
		rule: #'methodDefList' ]
		on: RwTonelParseRequireMethodCategoryNotification
		do: [ :ex | ex resume: '' ].
	self assert: parsed flattened size equals: 3
]

{ #category : 'tests' }
RwTonelParserTest >> testRemoveFromEnclosingStartEnd [
  | parser nl |
  nl := RwTonelParser lineEnding.
  parser := RwTonelParser new.
  self
    assert: (parser removeFrom: '[ ^ self ]' enclosingStart: $[ end: $])
    equals: '^ self'.
  self
    assert:
      (parser
        removeFrom:
          ('[ 
	^ self ]' withLineEndings: nl)
        enclosingStart: $[
        end: $])
    equals: Character tab asString , '^ self'.
  self
    assert:
      (parser
        removeFrom:
          ('[ 
	^ self
	]' withLineEndings: nl)
        enclosingStart: $[
        end: $])
    equals: Character tab asString , '^ self'.
  self
    assert:
      (parser
        removeFrom:
          ('[ 

	^ self

]' withLineEndings: nl)
        enclosingStart: $[
        end: $])
    equals:
      ('
	^ self
' withLineEndings: nl)

]

{ #category : 'tests' }
RwTonelParserTest >> testTraitTypeDef [
	self
		assertParse:
			'
Trait { 
	#name: ''X'',
	#category: ''Z'' 
}'
		rule: #'typeDef'
		equals:
			(RwTraitDefinition
				newForTraitNamed: 'X'
				instanceVariableNames: #()
				packageName: 'Z')
]

{ #category : 'tests' }
RwTonelParserTest >> testType [
	self assertParse: 'Class' rule: #type equals: 'Class'.
	self assertParse: 'Trait' rule: #type equals: 'Trait'.
	self assertParse: 'Extension' rule: #type equals: 'Extension'

]

{ #category : 'tests' }
RwTonelParserTest >> testTypeDef [
  self
    assertParse:
      '
"
this is a test
"
Class { 
	#name: ''X'',
	#superclass: ''Y'',
	#category: ''Z'' 
}'
    rule: #'typeDef'
    equals:
      (self
        newClassDefinitionForClassNamed: 'X'
        superclassName: 'Y'
        category: 'Z'
        instVarNames: #()
        classVarNames: #()
        poolDictionaryNames: #()
        classInstVarNames: #()
        type: #'normal'
        comment: 'this is a test')

]

{ #category : 'tests' }
RwTonelParserTest >> testTypeDefWithClassVars [
  self
    assertParse:
      '
"
this is a test
"
Class {
	#name : #MCMockASubclass,
	#superclass : #MCMockClassA,
	#instVars : [
		''x''
	],
	#classVars : [
		''Y''
	],
	#category : #MonticelloMocks
}
'
    rule: #'typeDef'
    equals:
      (self
        newClassDefinitionForClassNamed: 'MCMockASubclass'
        superclassName: 'MCMockClassA'
        category: 'MonticelloMocks'
        instVarNames: #(#'x')
        classVarNames: #(#'Y')
        poolDictionaryNames: #()
        classInstVarNames: #()
        type: #'normal'
        comment: 'this is a test')

]
