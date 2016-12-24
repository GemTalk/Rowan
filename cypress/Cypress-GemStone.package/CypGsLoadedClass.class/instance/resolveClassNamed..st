"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: unknown
"
resolveClassNamed: aName
	"If the given name is bound to a class in the environment of the current session, 
	answer that class. Otherwise, answer nil."

	| resolved |
	resolved := GsCurrentSession currentSession objectNamed: aName.
	^(resolved isBehavior and: [resolved isMeta not])
		ifTrue: [resolved]
		ifFalse: [nil]