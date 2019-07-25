Class {
	#name : 'RBTonelScanner',
	#superclass : 'RBScanner',
	#category : 'AST-Core'
}

{ #category : 'private' }
RBTonelScanner >> peekTonelFor: aCharacterType value: aCharacterValue [

	^ currentCharacter = aCharacterValue and: [ characterType =  aCharacterType ]
]

{ #category : 'error handling' }
RBTonelScanner >> stream [
	^stream
]

{ #category : 'accessing' }
RBTonelScanner >> tonelNext [
	"only used when parsing the tonel method selector line"

	| token |
(characterType == #special and: [currentCharacter = $[ ])
	ifFalse: [ 
		 characterType ~~ #eof
			ifTrue: [ self error: 'Cannot parse Tonel method body. Missing ''[''.' ] ].
	buffer reset.
	tokenStart := stream position.
	token := characterType = #eof 
				ifTrue: 
					[RBToken start: tokenStart + 1	"The EOF token should occur after the end of input"]
				ifFalse: [self scanToken].
	self stripSeparators.
"don't read comments"
	false ifTrue: [ token comments: self getComments ].
"skip the $[ and get next token"
	^self next
]
