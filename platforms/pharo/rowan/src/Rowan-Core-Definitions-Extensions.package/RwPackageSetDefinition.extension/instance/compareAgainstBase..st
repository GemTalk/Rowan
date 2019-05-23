*rowan-core-definitions-extensions
compareAgainstBase: aDefinition

	| result |
	result := RwPackageSetModification new.
	self
		compareDictionary: definitions
		againstBaseDictionary: aDefinition definitions
		into: result
		elementClass: RwPackageDefinition.
	^ result
