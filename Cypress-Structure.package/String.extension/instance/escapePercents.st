*Cypress-Structure
escapePercents
	"Answer a new string with 'dangerous' characters escaped to their %XX form,
	 for use in HTTP transactions."

	^String streamContents: 
			[:stream |
			self do: 
					[:c |
					c isSafeForHTTP
						ifTrue: [stream nextPut: c]
						ifFalse: 
							[stream nextPut: $%.
							c codePoint // 16 printOn: stream base: 16 showRadix: false.
							c codePoint \\ 16 printOn: stream base: 16 showRadix: false]]]