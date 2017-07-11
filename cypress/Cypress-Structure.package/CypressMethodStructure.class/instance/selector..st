"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: accessing
"
selector: aString

	| stream |
	stream := WriteStreamPortable on: (String new: 100).
	aString
		do: [:chara | stream nextPut: (chara = $: ifTrue: [$.] ifFalse: [chara])].
	name := stream contents
