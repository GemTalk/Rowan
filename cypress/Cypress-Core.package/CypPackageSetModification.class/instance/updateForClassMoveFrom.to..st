"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private - moves
"
updateForClassMoveFrom: removal to: addition
	"Transform the given removal and addition to a move."

	| oldDefinition newDefinition classModification |
	oldDefinition := (removal classesModification
				modificationOf: removal classKey) before.
	newDefinition := (addition classesModification
				modificationOf: addition classKey) after.

	"Delete the removal and the addition."
	removal classesModification removeModificationOf: removal classKey.
	addition classesModification removeModificationOf: addition classKey.

	"Record the move."
	movedClasses add: (CypClassMove
				classBefore: oldDefinition
				classAfter: newDefinition
				packageBefore: removal packageDefinition
				packageAfter: addition packageDefinition).

	"Does the class have other modifications that need to be recorded?"
	classModification := newDefinition compareAgainstBase: oldDefinition.
	classModification isEmpty
		ifFalse: 
			[addition classesModification addElementModification: classModification]