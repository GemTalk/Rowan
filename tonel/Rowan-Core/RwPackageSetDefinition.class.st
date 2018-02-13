"
No class-specific documentation for CypPackageSetDefinition, hierarchy is: 
Object
  CypDefinition( properties)
    CypPackageSetDefinition( packages)

"
Class {
	#name : 'RwPackageSetDefinition',
	#superclass : 'RwDefinitionSetDefinition',
	#category : 'Rowan-Core'
}

{ #category : 'comparing' }
RwPackageSetDefinition >> compareAgainstBase: aDefinition [

	| result |
	result := RwPackageSetModification new.
	self
		compareDictionary: definitions
		againstBaseDictionary: aDefinition definitions
		into: result
		elementClass: RwPackageDefinition.
	result
		updateForClassMoves;
		updateForMethodMoves.
	^ result
]
