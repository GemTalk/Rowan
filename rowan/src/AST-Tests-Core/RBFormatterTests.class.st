Class {
	#name : 'RBFormatterTests',
	#superclass : 'TestCase',
	#category : 'AST-Tests-Core'
}

{ #category : 'private' }
RBFormatterTests >> formatClass: aClass [
	aClass selectors
		do: [ :each | self formatClass: aClass selector: each ]
]

{ #category : 'private' }
RBFormatterTests >> formatClass: aClass selector: aSymbol [
	self formatters
		do: [ :each | self formatClass: aClass selector: aSymbol formatter: each ]
]

{ #category : 'private' }
RBFormatterTests >> formatClass: aClass selector: aSymbol formatter: aFormatterClass [
	| source tree1 tree2 | 
	source := aClass sourceCodeAt: aSymbol.
	tree1 := RBParser parseMethod: source.
	tree2 := RBParser 
		parseMethod: (aFormatterClass new format: tree1)
		onError: [ :err :pos | self assert: false ].
	self assert: tree1 = tree2
]

{ #category : 'private' }
RBFormatterTests >> formatMethodSource: source expected: expectedArray [
  1 to: self formatters size do: [ :index | 
    | formatter |
    formatter := self formatters at: index.
    self
      formatMethodSource: source
      formatter: formatter
      expected: (expectedArray at: index) ]
]

{ #category : 'private' }
RBFormatterTests >> formatMethodSource: source formatter: aFormatterClass expected: expected [
  | tree1 tree2 x |
  tree1 := RBParser parseMethod: source.
  tree2 := RBParser
    parseMethod: (aFormatterClass new format: tree1)
    onError: [ :err :pos | self assert: false ].
  x := tree2 source.
  self assert: x = expected.
  self assert: tree1 = tree2
]

{ #category : 'accessing' }
RBFormatterTests >> formatters [
	^ Array with: RBFormatter with: RBConfigurableFormatter
]

{ #category : 'testing' }
RBFormatterTests >> testCoreSystem [
  #(#'Object' #'Behavior' #'Boolean' #'Integer' #'SmallInteger' #'Collection' #'String')
    do: [ :each | 
      | class |
      class := Rowan globalNamed: each.
      self
        formatClass: class;
        formatClass: class class ]
]

{ #category : 'testing' }
RBFormatterTests >> testIndexingSystem [
  #(#'IXCompoundQueryPredicateTests' #'IXGsClassicQueryBlockOptimizerTests' #'IXQueryBlockPredicateTests' #'IXQueryExpressionPrintStringTests' #'IXQueryExpressionTests' #'IXQueryPredicateTests')
    do: [ :each | 
       (Rowan globalNamed: each) ifNotNil: [ :class | 
          class ifNotNilself
            formatClass: class;
            formatClass: class class ] ]
]

{ #category : 'testing' }
RBFormatterTests >> testQueryBlocks [
  "GemStone query blocks must have () around predicates when more than one predicate is involved and the only operator between predicates is & ... formatter may be a little more conservative"

  self
    formatMethodSource: 'select: nsc nsc select: { :person | adam == adam }'
    expected:
      {'select: nsc 
	nsc select: {:person | adam == adam}'.
      'select: nsc
  nsc select: { :person | adam == adam }'}.
  self
    formatMethodSource:
      'select: nsc nsc select: { :person | (true) & (1 < person) & (person <= 3) }'
    expected:
      {'select: nsc 
	nsc select: {:person | (true) & (1 < person) & (person <= 3)}'.
      'select: nsc
  nsc select: { :person | true & (1 < person) & (person <= 3) }'}.
  self
    formatMethodSource:
      'select: nsc nsc select: { :person | (person.isMale) & (1 < person.numberOfChildren) & (person.numberOfChildren <= 3) }'
    expected:
      {'select: nsc 
	nsc select: 
			{:person | 
			(person.isMale) & (1 < person.numberOfChildren) 
				& (person.numberOfChildren <= 3)}'.
      'select: nsc
  nsc
    select: { :person | (person.isMale) & (1 < person.numberOfChildren) & (person.numberOfChildren <= 3) }'}.
  self
    formatMethodSource:
      'select: nsc nsc select: { :person | (person.isMale == true) & (1 < person.numberOfChildren) & (person.numberOfChildren <= 3) }'
    expected:
      {'select: nsc 
	nsc select: 
			{:person | 
			(person.isMale == true) & (1 < person.numberOfChildren) 
				& (person.numberOfChildren <= 3)}'.
      'select: nsc
  nsc
    select: { :person | 
      (person.isMale == true) & (1 < person.numberOfChildren)
        & (person.numberOfChildren <= 3) }'}.
  self
    formatMethodSource:
      'select: nsc nsc select: {:person | (person.isMale) & (person.firstName = ''Casey'')}'
    expected:
      {'select: nsc 
	nsc select: {:person | (person.isMale) & (person.firstName = ''Casey'')}'.
      'select: nsc
  nsc select: { :person | (person.isMale) & (person.firstName = ''Casey'') }'}
]
