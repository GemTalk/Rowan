"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: *Cypress-GemStoneCompatibilityLayer
"
escapePercents
	"Answer a new string with 'dangerous' characters escaped to their %XX form,
	 for use in HTTP transactions."

	| stream |
	stream := WriteStreamPortable on: (String new: 100).
	self do: 
			[:each |
			| c |
			(c := Character withValue: each) isSafeForHTTP
				ifTrue: [stream nextPut: c]
				ifFalse: 
					[stream nextPut: $%.
					each // 16 printOn: stream base: 16 showRadix: false.
					each \\ 16 printOn: stream base: 16 showRadix: false]].
	^stream contents