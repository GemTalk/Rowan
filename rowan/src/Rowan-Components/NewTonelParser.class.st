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
	| type |
	
	type := self untilIncluding: '>>'.
	methodParser := RBParser new.
	methodParser
		scanner: (methodParser scannerClass on: stream
				errorBlock: [:errorMessage :errorPosition |self halt]).
	currentMethodNode := methodParser parseTonelMessagePattern.
	methodBodyStart := methodParser scanner previousStepPosition - 2.
GsFile stderr nextPutAll: 'pos: ', methodBodyStart printString; lf.
	type := type trimBoth substrings: ' '.
	type size = 1 ifTrue: [ type := type copyWith: nil ].
	^ { 
		type.
		currentMethodNode selector.
	}
]

{ #category : 'parsing' }
NewTonelParser >> methodBody [
	| source end count currentPosition window |
	methodParser parseTonelPragmas.
	currentMethodNode body: (methodParser parseTonelStatements: true).
	(methodParser currentToken isSpecial and: [ methodParser currentToken value = $]])
		ifFalse: [ self error: 'Cannot parse Tonel method. Missing '']''.'].
	end := methodParser currentToken start - 2.
	count :=  end - methodBodyStart.
window := methodParser scanner stream contents copyFrom: methodBodyStart  to: methodBodyStart + count.
	source := String new: count.
	currentPosition := methodParser scanner stream position.
	methodParser scanner stream position: methodBodyStart -1.
	methodParser scanner stream readInto: source startingAt: 1 count: count + 1.
	methodParser scanner stream position: currentPosition.
self halt.
	^ source
]
