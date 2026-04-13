Class {
	#name : 'RwStrictTonelParser',
	#superclass : 'RwTonelParser',
	#instVars : [
		'sawClassMethods',
		'sawInstanceMethods',
		'checkTonelInstanceSelectors',
		'checkTonelClassSelectors',
		'methodSortOrder'
	],
	#category : 'Rowan-Tonel-Core'
}

{ #category : 'other' }
RwStrictTonelParser >> _checkMethodOrderFor: className selector: selector isMeta: isMeta [
	| col |
	col := isMeta
		ifTrue: [ 
			self sawInstanceMethods
				ifTrue: [ 
					self
						error:
							'The  method ' , className
								,
									(isMeta
										ifTrue: [ ' class' ]
										ifFalse: [ '' ]) , ' >> ' , selector
								,
									((packageReader respondsTo: #'currentPackageDefinition')
										ifTrue: [ 
											"Use #perform: to avoid triggering a sent but implemented error -- Note #respondsTo: is used to avoid MNU"
											' in package '
												, (packageReader perform: #'currentPackageDefinition' name) ]
										ifFalse: [ '' ])
								,
									' is out of order - class methods must come before instance methods in a tonel class file.' ].
			self sawClassMethods: true.
			self checkTonelClassSelectors ]
		ifFalse: [ 
			self sawInstanceMethods: true.
			self checkTonelInstanceSelectors ].
	col add: selector.
	col last = selector
		ifFalse: [ 
			self
				error:
					'The method ' , className
						,
							(isMeta
								ifTrue: [ ' class >>' ]
								ifFalse: [ ' >>' ]) , selector , ' is not in canonical order.' ]
]

{ #category : 'accessing' }
RwStrictTonelParser >> checkTonelClassSelectors [
	^ checkTonelClassSelectors
		ifNil: [ 
			self methodSortOrder = 'codePoint'
				ifTrue: [ 
					checkTonelClassSelectors := SortedCollection
						sortBlock: [ :a :b | (a  codePointCompareTo:  b) < 1 ] ]
				ifFalse: [ 
					"unicode method sort order"
					checkTonelClassSelectors := SortedCollection
						sortBlock: [ :a :b | a _unicodeLessThan: b  ] ] ]
]

{ #category : 'accessing' }
RwStrictTonelParser >> checkTonelInstanceSelectors [
	^ checkTonelInstanceSelectors
		ifNil: [ 
			self methodSortOrder = 'codePoint'
				ifTrue: [ 
					checkTonelInstanceSelectors := SortedCollection
						sortBlock: [ :a :b | (a  codePointCompareTo:  b) < 1  ] ]
				ifFalse: [ 
					checkTonelInstanceSelectors := SortedCollection
						sortBlock: [ :a :b | a _unicodeLessThan: b ] ] ]
]

{ #category : 'parsing' }
RwStrictTonelParser >> method [
	| type selector selectorChunk |
	"if there is a single $> then we read until we find the NEXT ONE ..."
	type := self readClassNameForMethod.
	"if there is no leading $[ we read until we find the NEXT ONE"
	selectorChunk := (stream upTo: $[) trimBoth.
	(selectorChunk includes: $]) 
		ifTrue: [ 
			"If a ] precedes the initial $], then thats as good as a missing $["
			RwTonelParseError signal: 'Misplaced or missing ''['' in tonel file' ].
	stream position: stream position - 1.	"in-lined untilExcludingChar: ... need to preserve the $[ in the selectorChunk"
	selector := self cleanSelector: selectorChunk.
	[Behavior extractSelector: selector] 
		on: CompileError 
		do: [:ex | 
			"if there is a compile error extracting the selector, then thats as good as a missing $["
			RwTonelParseError signal: 'Misplaced or missing ''['' in tonel file' ].
	type := type trimBoth substringsSpace.	"substrings: ' ' "
		type size = 1
		ifTrue: [ type := type copyWith: nil ].
	lastSelectorParsed := selector.
	^ {type.
	selector}
]

{ #category : 'accessing' }
RwStrictTonelParser >> methodSortOrder [
	^ methodSortOrder
		ifNil: [ 
			methodSortOrder := (packageReader respondsTo: #'currentProjectDefinition')
				ifTrue: [ 
					"Use #perform: to avoid triggering a sent but implemented error -- Note #respondsTo: is used to avoid MNU"
					(packageReader perform: #'currentProjectDefinition') methodSortOrder ]
				ifFalse: [ 
					(packageReader respondsTo: #'methodSortOrder')
						ifTrue: [ packageReader methodSortOrder ]
						ifFalse: [ 'unicode' ] ] ]
]

{ #category : 'accessing' }
RwStrictTonelParser >> methodSortOrder: object [
	methodSortOrder := object
]

{ #category : 'private factory' }
RwStrictTonelParser >> newMethodDefinitionFrom: anArray [
	| metadata className meta selector source categ fourth fourthTwo selsSet |
	metadata := (anArray at: 2) ifNil: [ Dictionary new ].
	className := ((fourth := anArray at: 4) at: 1) at: 1.
	[ Metaclass3 _validateNewClassName: className asSymbol ]
		on: Error
		do: [ :ex | self error: 'Invalid class name ' , className printString ].
	meta := ((fourth at: 1) at: 2) ~~ nil.
	selector := self extractSelector: (fourthTwo := fourth at: 2) trimBoth.
	selsSet := meta
		ifTrue: [ 
			selectors ifNil: [ selectors := SymbolSet new ].
			selectors ]
		ifFalse: [ 
			classSelectors ifNil: [ classSelectors := SymbolSet new ].
			classSelectors ].
	(selsSet includes: selector)
		ifTrue: [ self error: 'Duplicate selector ' , selector ].
	selsSet add: selector.
	self _checkMethodOrderFor: className selector: selector isMeta: meta.
	source := String new.
	source addAll: fourthTwo.
	(anArray at: 5) ifNotEmpty: [ :src | source addAll: src ].
	categ := metadata
		at: #'category'
		ifAbsent: [ 
			"to avoid error, resume with default category string"
			(RwTonelParseRequireMethodCategoryNotification
				className: className
				isMeta: meta
				selector: selector) signal ].

	^ self packageReader
		newMethodDefinitionForClassNamed: className
		classIsMeta: meta
		selector: selector
		category: categ
		source: source
]

{ #category : 'accessing' }
RwStrictTonelParser >> sawClassMethods [
	^ sawClassMethods ifNil: [ sawClassMethods := false ]
]

{ #category : 'accessing' }
RwStrictTonelParser >> sawClassMethods: object [
	sawClassMethods := object
]

{ #category : 'accessing' }
RwStrictTonelParser >> sawInstanceMethods [
	^ sawInstanceMethods ifNil: [ sawInstanceMethods := false ]
]

{ #category : 'accessing' }
RwStrictTonelParser >> sawInstanceMethods: object [
	sawInstanceMethods := object
]
