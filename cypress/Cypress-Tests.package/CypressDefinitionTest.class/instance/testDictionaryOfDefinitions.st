"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: tests
"
testDictionaryOfDefinitions

	| dict |
	"baseDefinitions"
	dict := Dictionary new.
	self baseDefinitions do: [:each | 
		dict at: each put: each ].
	self baseDefinitions do: [:each | 
		self assert: (dict at: each) equals: each ].

	"targetDefinitions"
	dict := Dictionary new.
	self targetDefinitions do: [:each | 
		dict at: each put: each ].
	self targetDefinitions do: [:each | 
		self assert: (dict at: each) equals: each ].
