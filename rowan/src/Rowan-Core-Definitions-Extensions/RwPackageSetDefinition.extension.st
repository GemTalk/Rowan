Extension { #name : 'RwPackageSetDefinition' }

{ #category : '*rowan-core-definitions-extensions' }
RwPackageSetDefinition >> compareAgainstBase_forUpgrade: aDefinition [

	| result |
	result := RwPackageSetModification new.
	self
		compareDictionary: definitions
		againstBaseDictionary_forUpgrade: aDefinition definitions
		into: result
		elementClass: RwPackageDefinition.
	^ result
]

{ #category : '*rowan-core-definitions-extensions' }
RwPackageSetDefinition >> compareAgainstBase: aDefinition [

	| result |
	result := RwPackageSetModification new.
	self
		compareDictionary: definitions
		againstBaseDictionary: aDefinition definitions
		into: result
		elementClass: RwPackageDefinition.
	^ result

]
