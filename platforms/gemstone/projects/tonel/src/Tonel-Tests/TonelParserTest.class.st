Class {
	#name : 'TonelParserTest',
	#superclass : 'TestCase',
	#category : 'Tonel-Tests'
}

{ #category : 'Testing' }
TonelParserTest class >> isAbstract [
  "Override to true if a TestCase subclass is Abstract and should not have
	TestCase instances built from it"

  ^ self sunitName = #'TonelParserTest'

]

{ #category : 'asserting' }
TonelParserTest >> assertParse: aString rule: rule equals: result [ 
	self 
		assert: (self parse: aString rule: rule)
		equals: result

]

{ #category : 'private' }
TonelParserTest >> newClassDefinitionForClassNamed: nameString superclassName: superclassString category: categoryString instVarNames: ivarArray classVarNames: cvarArray poolDictionaryNames: poolArray classInstVarNames: civarArray type: typeSymbol comment: commentString [
  self subclassResponsibility

]

{ #category : 'private' }
TonelParserTest >> newClassDefinitionFrom: anArray [
  self subclassResponsibility

]

{ #category : 'private' }
TonelParserTest >> newMethodDefinitionForClassNamed: className classIsMeta: meta selector: selector category: category source: source [
  self subclassResponsibility

]

{ #category : 'private' }
TonelParserTest >> newTraitDefinitionFrom: anArray [
  self subclassResponsibility

]

{ #category : 'private' }
TonelParserTest >> newTypeDefinitionFrom: anArray [
  self subclassResponsibility

]

{ #category : 'private' }
TonelParserTest >> parse: aString rule: rule [
  | parser |
  parser := TonelParser onString: aString forReader: self.
  ^ parser perform: rule

]

{ #category : 'asserting' }
TonelParserTest >> shouldParse: aString rule: rule raise: error [
  | parser |
  parser := TonelParser onString: aString  forReader: self.
  self should: [ parser perform: rule ] raise: error

]

{ #category : 'tests' }
TonelParserTest >> testComment [
	self 
		shouldParse: 'this "should" be an error'
		rule: #comment
		raise: TonelParseError.
	
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
TonelParserTest >> testExtractSelector [
	| parser |
	
	parser := TonelParser new.
	
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
TonelParserTest >> testMetadata [
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
TonelParserTest >> testMethod [
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
TonelParserTest >> testMethodBody [
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
TonelParserTest >> testMethodDef [
	self 
		assertParse: '
{ #category: ''accessing'' }
Object>>name [
	^ self printString
]'
		rule: #methodDef
		equals: (self newMethodDefinitionForClassNamed: #Object
			classIsMeta: false
			selector: #name
			category: 'accessing' 
			source: 'name
	^ self printString').
			
	self 
		assertParse: '
Object class>>name [ 
	^ self printString
]'
		rule: #methodDef
		equals: (self newMethodDefinitionForClassNamed: #Object
			classIsMeta: true
			selector: #name
			category: ''
			source: 'name 
	^ self printString').

	self 
		assertParse: '
TClass classSide >> template: aSystemCategoryName [ 
	"I really do not care"
]'
		rule: #methodDef
		equals: (self newMethodDefinitionForClassNamed: #TClass
			classIsMeta: true
			selector: #template:
			category: ''
			source: 'template: aSystemCategoryName 
	"I really do not care"').

]

{ #category : 'tests' }
TonelParserTest >> testMethodDefList [
	| parsed |
	
	parsed := self 
		parse: '
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
		rule: #methodDefList.
		
	self assert: parsed flattened size equals: 3

]

{ #category : 'tests' }
TonelParserTest >> testRemoveFromEnclosingStartEnd [
  | parser nl |
  nl := TonelWriter lineEnding.
  parser := TonelParser new.
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
TonelParserTest >> testType [
	self assertParse: 'Class' rule: #type equals: 'Class'.
	self assertParse: 'Trait' rule: #type equals: 'Trait'.
	self assertParse: 'Extension' rule: #type equals: 'Extension'

]

{ #category : 'tests' }
TonelParserTest >> testTypeDef [
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
TonelParserTest >> testTypeDefWithClassVars [
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
