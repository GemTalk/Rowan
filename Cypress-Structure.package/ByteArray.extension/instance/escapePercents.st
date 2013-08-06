"
notice: This work is protected by copyright. All rights reserved.
category: *Cypress-Structure
"
escapePercents
	"Answer a new string with 'dangerous' characters escaped to their %XX form,
	 for use in HTTP transactions."

	^String streamContents: 
			[:stream |
			self do: 
					[:each |
					| c |
					(c := Character withValue: each) isSafeForHTTP
						ifTrue: [stream nextPut: c]
						ifFalse: 
							[stream nextPut: $%.
							each // 16 printOn: stream base: 16 showRadix: false.
							each \\ 16 printOn: stream base: 16 showRadix: false]]]