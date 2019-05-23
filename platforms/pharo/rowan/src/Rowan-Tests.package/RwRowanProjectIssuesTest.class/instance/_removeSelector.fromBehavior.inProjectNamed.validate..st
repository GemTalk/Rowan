private-issue 24
_removeSelector: selector fromBehavior: theBehavior inProjectNamed: projectName validate: validate

	"validate the dirty state of project before and after method is removed"

	| project |

	project := RwProject newNamed: projectName.

	validate ifTrue: [ self deny: project isDirty ].

	theBehavior rwRemoveSelector: selector.

	validate ifTrue: [ self assert: project isDirty ].
