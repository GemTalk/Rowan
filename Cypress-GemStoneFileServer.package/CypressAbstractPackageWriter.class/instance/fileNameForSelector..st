"
notice: This work is protected by copyright. All rights reserved.
category: private
"
fileNameForSelector: selector

	^selector last = $:
		ifTrue: [selector copyReplacing: $: with: $.]
		ifFalse: 
			[(selector first isLetter or: [selector first = $_])
				ifTrue: [selector]
				ifFalse: 
					[| specials |
					specials := self class specials.
					String streamContents: 
							[:output |
							output nextPut: $^.
							selector do: [:each | output nextPutAll: (specials at: each)]
								separatedBy: [output nextPut: $.]]]]
