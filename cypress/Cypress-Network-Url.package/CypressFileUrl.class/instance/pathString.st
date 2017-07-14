"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: paths
"
pathString
	"Path as it appears in a URL with $/ as delimiter."

	| s first |
	s := WriteStreamPortable on: (String new: 100).

	"isAbsolute ifTrue:[ s nextPut: $/ ]."
	first := true.
	self path do: 
			[:p |
			first ifFalse: [s nextPut: $/].
			first := false.
			self writeWithHttpEscapes: p on: s].
	^s contents
