private-issue 24
_writeProjectNamed: projectName validate: validate

	"validate the dirty state of project before and after write"

	| project |

	project := RwProject newNamed: projectName.

	validate ifTrue: [ self assert: project isDirty ]. "a project is dirty if it has changes that are not written to disk"

	Rowan projectTools write writeProjectNamed: projectName.

	validate ifTrue: [ self deny: project isDirty ]. "a project is not dirty if it has been written to disk"
