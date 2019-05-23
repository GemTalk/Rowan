*rowan-core-definitions-extensions
compareAgainstBase_254: aDefinition

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
