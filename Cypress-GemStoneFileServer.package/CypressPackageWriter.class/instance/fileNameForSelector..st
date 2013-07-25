private
fileNameForSelector: selector

	^selector last = $:
		ifTrue: [selector copyReplacing: $: with: $.]
		ifFalse: 
			[selector first isLetter
				ifTrue: [selector]
				ifFalse: 
					[| specials |
					specials := self class specials.
					String streamContents: 
							[:output |
							output nextPut: $^.
							selector do: [:each | output nextPutAll: (specials at: each)]
								separatedBy: [output nextPut: $.]]]]