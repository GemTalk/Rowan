Class {
	#name : 'NewTonelParser',
	#superclass : 'TonelParser',
	#instVars : [
		'methodParser',
		'methodNode'
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
				errorBlock: [self halt]).
	methodNode := methodParser parseTonelMessagePattern.
	type := type trimBoth substrings: ' '.
	type size = 1 ifTrue: [ type := type copyWith: nil ].
	^ { 
		type.
		methodNode selector.
	}
]

{ #category : 'parsing' }
NewTonelParser >> methodBody [
	| start end_1 end_2 buffer source window |
buffer := methodParser scanner stream buffer.
start := methodParser scanner stream position.
window:= buffer copyFrom: start-1 to: start+1.
	methodParser parseTonelPragmas.
	methodNode body: (methodParser parseTonelStatements: true).
end_1 := methodParser scanner stream position.
end_2 := methodParser scanner previousStepPosition.
buffer := methodParser scanner stream buffer.
(methodParser currentToken isSpecial and: [ methodParser currentToken value = $]])
	ifFalse: [ self error: 'Cannot parse Tonel method. Missing '']''.'].
source := buffer copyFrom: start to: methodParser currentToken start - 2.
self halt.
]
