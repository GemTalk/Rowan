Extension { #name : 'RwProjectSetDefinition' }

{ #category : '*rowan-core-definitions-extensions' }
RwProjectSetDefinition >> compareAgainstBase_254: aDefinition [

	| result |
	result := RwProjectSetModification new.
	self
		compareDictionary: definitions
		againstBaseDictionary: aDefinition definitions
		into: result
		elementClass: RwProjectDefinition.
	result
		updateForClassMoves;
		updateForMethodMoves.
	^ result

]

{ #category : '*rowan-core-definitions-extensions' }
RwProjectSetDefinition >> compareAgainstBase_forUpgrade: aDefinition [

	| result |
	result := RwProjectSetModification new.
	self
		compareDictionary: definitions
		againstBaseDictionary_forUpgrade: aDefinition definitions
		into: result
		elementClass: RwProjectDefinition.
	^ result
]

{ #category : '*rowan-core-definitions-extensions' }
RwProjectSetDefinition >> compareAgainstBase: aDefinition [

	| result |
	result := RwProjectSetModification new.
	self
		compareDictionary: definitions
		againstBaseDictionary: aDefinition definitions
		into: result
		elementClass: RwProjectDefinition.
	^ result
]
