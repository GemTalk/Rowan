"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: comparing
"
comparePropertiesAgainstBase: aDefinition

	| keys modification |
	modification := CypPropertiesModification new.
	keys := properties keys copy.
	keys addAll: aDefinition properties keys.
	keys do: 
			[:key |
			| before after |
			before := aDefinition propertyAt: key ifAbsent: [nil].
			after := self propertyAt: key ifAbsent: [nil].
			before = after
				ifFalse: 
					[modification addElementModification: (CypPropertyModification
								key: key
								oldValue: before
								newValue: after)]].
	^modification