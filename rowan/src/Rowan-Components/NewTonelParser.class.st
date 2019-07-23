Class {
	#name : 'NewTonelParser',
	#superclass : 'TonelParser',
	#instVars : [
		'methodParser',
		'currentMethodNode'
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
	type := type trimBoth substrings: ' '.
	type size = 1 ifTrue: [ type := type copyWith: nil ].
	^ { 
		type.
		currentMethodNode selector.
	}
]

{ #category : 'parsing' }
NewTonelParser >> methodBody [
	| start source end count |
	start := methodParser scanner stream position.
	methodParser parseTonelPragmas.
	currentMethodNode body: (methodParser parseTonelStatements: true).
	(methodParser currentToken isSpecial and: [ methodParser currentToken value = $]])
		ifFalse: [ self error: 'Cannot parse Tonel method. Missing '']''.'].
	end := methodParser currentToken start - 2.
	count :=  end - start + 1.
	source := String new: count.
	methodParser scanner stream readInto: source startingAt: start count: count.
	^ source
]
