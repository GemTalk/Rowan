Extension { #name : 'RwProjectSetDefinition' }

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

{ #category : '*rowan-core-definitions-extensions' }
RwProjectSetDefinition >> compareAgainstBaseForLoader: aDefinition [
	"use this message IFF you are prepared to deal directly with movedClasses and movedMethods.
		For general purpose comparison, compareAgainstBase: is a better choice"

	| result |
	result := RwProjectSetModification new.
	self
		compareDictionary: definitions
		againstBaseDictionary: aDefinition definitions
		into: result
		elementClass: RwProjectDefinition.
	result
		updateForPackageMoves;
		updateForClassMoves;
		updateForTraitMoves;
		updateForMethodMoves;
		updateForTraitMethodMoves.

	^ result
]
