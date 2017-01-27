"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: comparing
"
compareAgainstBase: aDefinition

	| modification mod |
	modification := CypModificationSet new.
	mod := self comparePropertiesAgainstBase: aDefinition.
	mod isEmpty ifFalse: [modification addAll: mod].
	source = aDefinition source
		ifFalse: 
			[modification add: ((CypSourceModification new)
						before: aDefinition source;
						after: source;
						yourself)].
	^modification