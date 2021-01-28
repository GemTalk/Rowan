Extension { #name : 'RwDefinition' }

{ #category : '*rowan-core-definitions-extensions' }
RwDefinition >> comparePropertiesAgainstBase: aDefinition [

	| keys modification |
	modification := RwPropertiesModification new.
	keys := self propertiesForCompare keys copy.
	keys addAll: aDefinition propertiesForCompare keys.
	keys do: 
			[:key |
				| before after |
				before := aDefinition propertyForCompareAt: key ifAbsent: [nil].
				after := self propertyForCompareAt: key ifAbsent: [nil].
				(self _compareProperty: key propertyVaue: before againstBaseValue: after)
					ifFalse: 
						[modification addElementModification: (RwPropertyModification
							key: key
							oldValue: before
							newValue: after)]].
	^modification
]
