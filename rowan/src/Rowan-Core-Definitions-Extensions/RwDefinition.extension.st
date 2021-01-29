Extension { #name : 'RwDefinition' }

{ #category : '*rowan-core-definitions-extensions' }
RwDefinition >> comparePropertiesAgainstBase: aDefinition [

	| keys modification |
	modification := RwPropertiesModification new.
	keys := properties keys . "properties is a dictionary, keys is a copy"
  aDefinition properties keysDo:[:k | keys add: k ].
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

]
