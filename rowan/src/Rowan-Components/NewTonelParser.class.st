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

{ #category : 'parsing' }
NewTonelParser >> method [
	| type start end currentPosition selector count ar  |
	
	type := self untilIncluding: '>>'.
	methodParser := RBParser new.
	methodParser
		scanner: (methodParser scannerClass on: stream
				errorBlock: [:errorMessage :errorPosition |self halt]).
self halt.
	ar := methodParser parseTonelMessagePattern.
currentMethodNode :=  ar at: 1.
	methodBodyStart := methodParser scanner previousStepPosition - 2.
self halt.
start := methodParser scanner previousStepPosition.
	currentMethodNode := methodParser b.
end := methodParser scanner previousStepPosition.
	currentPosition := methodParser scanner stream position.
	methodParser scanner stream position: start -1.
count := end - start + 1.
selector := String new: count.
	methodParser scanner stream readInto: selector startingAt: 1 count: count.
	methodParser scanner stream position: currentPosition.

	type := type trimBoth substrings: ' '.
	type size = 1 ifTrue: [ type := type copyWith: nil ].
	^ { 
		type.
		ar at: 2.
	}
]

{ #category : 'parsing' }
NewTonelParser >> methodBody [
	| source end count currentPosition |

	(methodParser currentToken isSpecial and: [ methodParser currentToken value = $[ ])
		ifFalse: [ self error: 'Cannot parse Tonel method body. Missing ''[''.' ].
	self step.	"skip the $["
	methodBodyStart := methodParser scanner previousStepPosition - 2.
	methodParser parseTonelPragmas.
	currentMethodNode body: (methodParser parseTonelStatements: true).
	(methodParser currentToken isSpecial and: [ methodParser currentToken value = $]])
		ifFalse: [ self error: 'Cannot parse Tonel method. Missing '']''.'].
	end := methodParser currentToken start - 2.
	count :=  end - methodBodyStart.
	source := String new: count.
	currentPosition := methodParser scanner stream position.
	methodParser scanner stream position: methodBodyStart -1.
	methodParser scanner stream readInto: source startingAt: 1 count: count + 1.
	methodParser scanner stream position: currentPosition.
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
		s << anArray fourth second.
		anArray fifth ifNotEmpty: [ :src | 
			s lf.
			s << src ] ].

	^ self packageReader newMethodDefinitionForClassNamed: className
		classIsMeta: meta
		selector: selector
		category: (metadata at: #category ifAbsent: [ '' ]) 
		source: source
]
