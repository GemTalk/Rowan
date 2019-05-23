*rowan-core-definitions-extensions
comparePropertiesAgainstBase: aDefinition

	| keys modification |
	modification := RwPropertiesModification new.
	keys := properties keys copy.
	keys addAll: aDefinition properties keys.
	keys do: 
			[:key |
				| before after |
				before := aDefinition propertyAt: key ifAbsent: [nil].
				after := self propertyAt: key ifAbsent: [nil].
				(self _compareProperty: key propertyVaue: before againstBaseValue: after)
					ifFalse: 
						[modification addElementModification: (RwPropertyModification
							key: key
							oldValue: before
							newValue: after)]].
	^modification
