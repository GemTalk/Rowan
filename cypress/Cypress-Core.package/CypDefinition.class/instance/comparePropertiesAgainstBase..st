"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: comparing
"
comparePropertiesAgainstBase: aDefinition

	| keys modification |
	modification := CypModificationSet new.
	keys := properties keys copy addAll: aDefinition properties keys.
	keys do: 
			[:key |
			| before after |
			before := self propertyAt: key ifAbsent: [nil].
			after := aDefinition propertyAt: key ifAbsent: [nil].
			before = nil
				ifTrue: [modification add: (CypAddition of: key -> after)]
				ifFalse: 
					[after = nil
						ifTrue: [modification add: (CypDeletion of: key -> before)]
						ifFalse: 
							[before = after
								ifFalse: 
									[modification
										add: (CypPropertyModification key: key before: before after: after)]]]].
	^modification