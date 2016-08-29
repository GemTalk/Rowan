"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: paths
"
pathString
	"Path as it appears in a URL with $/ as delimiter."
	
	
	^String streamContents: [ :s | | first |
		"isAbsolute ifTrue:[ s nextPut: $/ ]."
		first := true.
		self path do: [ :p |
			first ifFalse: [ s nextPut: $/ ].
			first := false.
			s nextPutAll: p encodeForHTTP ] ]