Class {
	#name : 'NewTonelParser',
	#superclass : 'TonelParser',
	#instVars : [
		'methodParser',
		'currentMethodNode',
		'methodBodyStart'
	],
	#category : 'Rowan-Components'
}

{ #category : 'private' }
NewTonelParser >> extractCommentsFor:  aNode from: originalSource on: codeStream [
	aNode comments do: 
			[:each | 
			codeStream
				space;
				nextPutAll: (originalSource copyFrom: each first to: each last)].
]

{ #category : 'parsing' }
NewTonelParser >> method [

	| type start end currentPosition selector count |
	
	type := self untilIncluding: '>>'.
	start := stream position.
	methodParser := RBTonelParser new.
	methodParser
		scanner: (methodParser scannerClass on: stream
				errorBlock: [:errorMessage :errorPosition |self halt]).
	currentMethodNode := methodParser parseTonelMessagePattern.
	end := methodParser scanner previousStepPosition.
	currentPosition := methodParser scanner stream position.

	methodParser scanner stream position: start.
	count := end - start.
	selector := String new: count.
	methodParser scanner stream readInto: selector startingAt: 1 count: count.
	methodParser scanner stream position: currentPosition.
	selector := selector trimBoth.
	type := type trimBoth substrings: ' '.
	type size = 1 ifTrue: [ type := type copyWith: nil ].
	methodBodyStart := stream position.
	methodParser tonelStep.	"eat the $["
	^ { 
		type.
		selector.
	}
]

{ #category : 'parsing' }
NewTonelParser >> methodBody [
	| codeStream source start end count currentPosition |
	codeStream := WriteStreamPortable on: String new.
	self extractCommentsFor:  methodParser currentToken from: stream contents on: codeStream.
	start := stream position.
	methodParser parseTonelPragmas.
	currentMethodNode body: (methodParser parseTonelStatements: true).
	end := methodParser currentToken start - 2.
	count :=  end - methodBodyStart.
	source := String new: count.
	currentPosition := methodParser scanner stream position.
	methodParser scanner stream position: methodBodyStart + 2.
	methodParser scanner stream readInto: source startingAt: 1 count: count + 1.
	methodParser scanner stream position: currentPosition.
self halt.
	^ source

]

{ #category : 'private factory' }
NewTonelParser >> newMethodDefinitionFrom: anArray [
	| metadata className meta selector source  |
	
	metadata := anArray second ifNil: [ Dictionary new ].
	className := anArray fourth first first.
	meta := anArray fourth first second notNil.
	selector := self extractSelector: anArray fourth second trimBoth.
	source := String streamContents: [ :s | 
		s << selector.
		anArray fifth ifNotEmpty: [ :src | 
			s lf.
			s << src ] ].

	^ self packageReader newMethodDefinitionForClassNamed: className
		classIsMeta: meta
		selector: selector
		category: (metadata at: #category ifAbsent: [ '' ]) 
		source: source
]
