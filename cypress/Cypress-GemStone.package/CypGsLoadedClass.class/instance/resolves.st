"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: testing
"
resolves
	"Does the class with my name still resolve to me?"

	| resolved |
	resolved := GsCurrentSession currentSession objectNamed: name.
	^resolved == handle