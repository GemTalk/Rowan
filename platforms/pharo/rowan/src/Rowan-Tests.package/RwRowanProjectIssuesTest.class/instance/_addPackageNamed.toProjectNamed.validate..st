private-issue 24
_addPackageNamed: packageName toProjectNamed: projectName validate: validate

	"validate the dirty state of project before and after add package call"

	| project |

	project := RwProject newNamed: projectName.

	validate ifTrue: [ self deny: project isDirty ].

	Rowan projectTools browser addPackageNamed: packageName toProjectNamed: projectName.

	validate ifTrue: [ self assert: project isDirty ].
