private - moves
updateForMethodMoveFrom: removal to: addition  isMeta: isMeta
	"Transform the given removal and addition to a move."

	| oldDefinition newDefinition methodModification |
	oldDefinition := (removal methodsModification
				modificationOf: removal methodKey) before.
	newDefinition := (addition methodsModification
				modificationOf: addition methodKey) after.

	"Delete the removal and the addition."
	removal methodsModification removeModificationOf: removal methodKey.
	addition methodsModification removeModificationOf: addition methodKey.

	"Record the move."
	movedMethods add: (RwMethodMove
				methodBefore: oldDefinition
				methodAfter: newDefinition
				classOrExtensionBefore: removal classDefinitionOrExtension
				classOrExtensionAfter: addition classDefinitionOrExtension
				packageBefore: removal packageDefinition
				packageAfter: addition packageDefinition
				projectBefore: removal projectDefinition
				projectAfter: addition projectDefinition
				isMeta: addition isMeta).

	"Does the method have other modifications that need to be recorded?"
	methodModification := newDefinition compareAgainstBase: oldDefinition.
	methodModification 
		isMeta: isMeta;
		classDefinition: addition classDefinitionOrExtension.
	methodModification isEmpty
		ifFalse: 
			[addition methodsModification addElementModification: methodModification]
