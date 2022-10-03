Extension { #name : 'RwPackageSetDefinition' }

{ #category : '*rowan-core-definitions-extensions' }
RwPackageSetDefinition >> compareAgainstBase: aDefinition [
	"return a package set modification sufficient to transform aDefinition into the reciever"

	| result |
	result := RwPackageSetModification new.
	self
		compareDictionary: definitions
		againstBaseDictionary: aDefinition definitions
		into: result
		elementClass: RwPackageDefinition.
	^ result
]
