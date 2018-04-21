Extension { #name : 'Symbol' }

{ #category : '*tonel-gemstone-kernel' }
Symbol >> keywords [

	"Answer an array of the keywords that compose the receiver."

	| kwd char keywords |
	keywords := Array new.
			kwd := WriteStreamPortable on: String new.
			1 to: self size do: [ :i | 
				kwd nextPut: (char := self at: i).
				char = $:
					ifTrue: [ 
						keywords add: kwd contents.
						kwd reset ] ].
			kwd position = 0
				ifFalse: [ keywords add: kwd contents ].
	(keywords size >= 1 and: [ (keywords at: 1) = ':' ])
		ifTrue: [ 
			"Has an initial keyword, as in #:if:then:else:"
			keywords := keywords allButFirst ].
	(keywords size >= 2 and: [ (keywords at: keywords size - 1) = ':' ])
		ifTrue: [ 
			"Has a final keyword, as in #nextPut::andCR"
			keywords := keywords
				copyReplaceFrom: keywords size - 1
				to: keywords size
				with: {(':' , keywords last)} ].
	^ keywords

]
