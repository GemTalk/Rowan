Class {
	#name : 'TonelWriterTest',
	#superclass : 'TestCase',
	#category : 'Tonel-Tests'
}

{ #category : 'Testing' }
TonelWriterTest class >> isAbstract [
  "Override to true if a TestCase subclass is Abstract and should not have
	TestCase instances built from it"

  ^ self sunitName = #'TonelWriterTest'

]

{ #category : 'private' }
TonelWriterTest >> creatClassDefinition: nameString superclassName: superclassString category: categoryString instVarNames: ivarArray classVarNames: cvarArray poolDictionaryNames: poolArray classInstVarNames: civarArray type: typeSymbol comment: commentString [
  self subclassResponsibility

]

{ #category : 'private' }
TonelWriterTest >> creatClassDefinition: nameString superclassName: superclassString traitComposition: traitCompositionString classTraitComposition: classTraitCompositionString category: categoryString instVarNames: ivarArray classVarNames: cvarArray poolDictionaryNames: poolArray classInstVarNames: civarArray type: typeSymbol comment: commentString [
  self subclassResponsibility

]

{ #category : 'private' }
TonelWriterTest >> creatMethodDefinition: classString classIsMeta: metaBoolean selector: selectorString category: catString source: sourceString [
  self subclassResponsibility

]

{ #category : 'private' }
TonelWriterTest >> creatMethodDefinition: classString selector: selectorString category: catString source: sourceString [
  ^ self
    creatMethodDefinition: classString
    classIsMeta: false
    selector: selectorString
    category: catString
    source: sourceString

]

{ #category : 'private' }
TonelWriterTest >> defaultPackageWriter [
  self subclassResponsibility

]

{ #category : 'tests' }
TonelWriterTest >> testSplitMethodSourceInto [
  | writer declaration source definition newLine tab space |
  newLine := TonelWriter lineEnding.
  tab := Character tab asString.
  space := Character space asString.
  writer := TonelWriter new.	"simplest split"
  definition := self creatMethodDefinition: #'SomeClass'
    selector: #'name'
    category: 'accessing'
    source:
      'name
	^ self'.
  writer
    splitMethodSource: definition
    into: [ :d :s | 
      declaration := d.
      source := s ].
  self assert: declaration equals: 'name'.
  self assert: source equals: newLine , tab , '^ self'.	"test space at the end of method declaration (it needs to be kept)"
  definition := self creatMethodDefinition: #'SomeClass'
    selector: #'name:'
    category: 'accessing'
    source:
      'name: aString 
	name := aString'.
  writer
    splitMethodSource: definition
    into: [ :d :s | 
      declaration := d.
      source := s ].
  self assert: declaration equals: 'name: aString'.
  self assert: source equals: space , newLine , tab , 'name := aString'.	"test multiline declaration"
  definition := self creatMethodDefinition: #'SomeClass'
    selector: #'method:with:several:lines:'
    category: 'accessing'
    source:
      'method: var1
	with: var2
	several: var3
	lines: var4
	
	^ var1 + var2 + var3 + var4'.
  writer
    splitMethodSource: definition
    into: [ :d :s | 
      declaration := d.
      source := s ].
  self
    assert: declaration
    equals:
      ('method: var1
	with: var2
	several: var3
	lines: var4'
        withLineEndings: TonelWriter lineEnding).
  self
    assert: source
    equals:
      (newLine , tab , newLine , tab , '^ var1 + var2 + var3 + var4'
        withLineEndings: newLine).	"test comment before declaration (it may happen, if someone copied from diffmorph)"
  definition := self creatMethodDefinition: #'SomeClass'
    selector: #'name:'
    category: 'accessing'
    source:
      '
"protocol: accessing"
name: aString 
	name := aString'.
  writer
    splitMethodSource: definition
    into: [ :d :s | 
      declaration := d.
      source := s ].
  self assert: declaration equals: 'name: aString'.
  self assert: source equals: space , newLine , tab , 'name := aString'.	"test source right after declaration (no enter between selector and source)"
  definition := self creatMethodDefinition: #'SomeClass'
    selector: #'name:'
    category: 'accessing'
    source: 'name: aString name := aString'.
  writer
    splitMethodSource: definition
    into: [ :d :s | 
      declaration := d.
      source := s ].
  self assert: declaration equals: 'name: aString'.
  self assert: source equals: ' name := aString'.	"test method name containin name of keywords in variables"
  definition := self creatMethodDefinition: #'SomeClass'
    selector: #'a:b:c:'
    category: 'accessing'
    source: 'a: b b: c c: d ^ 42'.
  writer
    splitMethodSource: definition
    into: [ :d :s | 
      declaration := d.
      source := s ].
  self assert: declaration equals: 'a: b b: c c: d'.
  self assert: source equals: ' ^ 42'

]

{ #category : 'tests' }
TonelWriterTest >> testWriteClassDefinitionOn [
  | writer def stream |
  writer := TonelWriter new
    packageWriter: self defaultPackageWriter;
    yourself.
  stream := TonelParser writeStreamClass on: String new.
  def := self
    creatClassDefinition: #'SomeObject'
    superclassName: #'Object'
    category: #'Kernel'
    instVarNames: #()
    classVarNames: #()
    poolDictionaryNames: #()
    classInstVarNames: #()
    type: #'normal'
    comment: 'comment test'.
  writer writeClassDefinition: def on: stream.
  self
    assert: stream contents
    equals:
      ('"
comment test
"
Class {
	#name : ''SomeObject'',
	#superclass : ''Object'',
	#category : ''Kernel''
}
'
        withLineEndings: TonelWriter lineEnding).
  stream := String new writeStreamPortable.
  def := self
    creatClassDefinition: #'SomeObject'
    superclassName: #'Object'
    category: #'Kernel'
    instVarNames: #(#'a' #'b' #'c')
    classVarNames: #(#'D' #'E')
    poolDictionaryNames: #(#'POOL')
    classInstVarNames: #(#'instVarA')
    type: #'normal'
    comment: 'comment test'.
  writer writeClassDefinition: def on: stream.
  self
    assert: stream contents
    equals:
      ('"
comment test
"
Class {
	#name : ''SomeObject'',
	#superclass : ''Object'',
	#instVars : [
		''a'',
		''b'',
		''c''
	],
	#classVars : [
		''D'',
		''E''
	],
	#pools : [
		''POOL''
	],
	#classInstVars : [
		''instVarA''
	],
	#category : ''Kernel''
}
'
        withLineEndings: TonelWriter lineEnding)

]

{ #category : 'tests' }
TonelWriterTest >> testWriteMethodDefinitionOn [
  | writer def stream |
  writer := TonelWriter new.
  stream := String new writeStreamPortable.
  def := self creatMethodDefinition: #'Object'
    classIsMeta: false
    selector: #'selector'
    category: 'accessing'
    source:
      'selector
	^ 42'.
  writer writeMethodDefinition: def on: stream.
  self
    assert: stream contents
    equals:
      ('
{ #category : ''accessing'' }
Object >> selector [
	^ 42
]
'
        withLineEndings: TonelWriter lineEnding).
  stream := String new writeStreamPortable.
  def := self creatMethodDefinition: #'Object'
    classIsMeta: true
    selector: #'selector'
    category: 'accessing'
    source:
      'selector
	^ 42'.
  writer writeMethodDefinition: def on: stream.
  self
    assert: stream contents
    equals:
      ('
{ #category : ''accessing'' }
Object class >> selector [
	^ 42
]
'
        withLineEndings: TonelWriter lineEnding).
  stream := String new writeStreamPortable.
  def := self creatMethodDefinition: #'Object'
    classIsMeta: false
    selector: #'='
    category: 'comparing'
    source:
      '= anObject
	^ self == anObject'.
  writer writeMethodDefinition: def on: stream.
  self
    assert: stream contents
    equals:
      ('
{ #category : ''comparing'' }
Object >> = anObject [
	^ self == anObject
]
'
        withLineEndings: TonelWriter lineEnding)

]

{ #category : 'tests' }
TonelWriterTest >> testWritePackageOn [
  | writer stream |
  writer := TonelWriter new.
  stream := TonelParser writeStreamClass on: String new.
  writer writePackage: 'ThePackage' on: stream.
  self
    assert: stream contents
    equals:
      ('Package { #name : ''ThePackage'' }
'
).

  stream := TonelParser writeStreamClass on: String new.
  writer writePackage: 'The-Package' on: stream.
  self
    assert: stream contents
    equals:
      ('Package { #name : ''The-Package'' }
'
).

  stream := TonelParser writeStreamClass on: String new.
  writer writePackage: 'The Package' on: stream.
  self
    assert: stream contents
    equals:
      ('Package { #name : ''The Package'' }
'
).

]
