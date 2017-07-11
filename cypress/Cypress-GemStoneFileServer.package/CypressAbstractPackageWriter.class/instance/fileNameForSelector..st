"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
fileNameForSelector: selector

	^selector last = $:
		ifTrue: [selector copyReplacing: $: with: $.]
		ifFalse: 
			[(selector first isLetter or: [selector first = $_])
				ifTrue: [selector]
				ifFalse: 
					[| specials output |
					specials := self class specials.
					output := WriteStreamPortable on: (String new: 100).
					output nextPut: $^.
					selector do: [:each | output nextPutAll: (specials at: each)]
						separatedBy: [output nextPut: $.].
					output contents]]
