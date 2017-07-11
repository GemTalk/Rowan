"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: accessing
"
selector

	| stream |
	stream := WriteStreamPortable on: (String new: 100).
	self name
		do: [:chara | stream nextPut: (chara = $. ifTrue: [$:] ifFalse: [chara])].
	^stream contents