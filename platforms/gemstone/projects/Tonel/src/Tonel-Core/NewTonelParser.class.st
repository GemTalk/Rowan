Class {
	#name : 'NewTonelParser',
	#superclass : 'TonelParser',
	#instVars : [
		'methodNode'
	],
	#category : 'Tonel-Core'
}

{ #category : 'parsing' }
NewTonelParser >> method [
	| type parser |
	
	type := self untilIncluding: '>>'.
	parser := RBParser new.
	parser
		scanner: (parser scannerClass on: stream
				errorBlock: [self halt]).
	methodNode := parser parseMessagePattern.
	type := type trimBoth substrings: ' '.
	type size = 1 ifTrue: [ type := type copyWith: nil ].
self halt.
	^ { 
		type.
		methodNode selector.
	}
]

{ #category : 'parsing' }
NewTonelParser >> methodBody [
	"I read a methodbody (what is inside [ ... ])
	 Since a method body can contain enclosing brackets we need to be sure we will skip them and
	 correctly read the method. For that, I have to take into account: 
		- I can mention [] in comments
		- I can mention [] in strings
		- I can use $[, $] 
		- I can have inner blocks
		- I can mention a comment of the form ""$"" or a comment of the form '$'
	 all that needs to be skipped "
	| result prevChar comment string count  parser |
	
	result := self class writeStreamClass on: String new.

	comment := false.
	string := false.
	prevChar := nil.
	count := 0.
	stream peek = $[ ifFalse: [ TonelParseError signal: 'Can''t parse method body' ].
	stream next. "eat $["
	parser := RBParser new.
	parser
		scanner: (parser scannerClass on: stream
				errorBlock: [self halt]).
]
