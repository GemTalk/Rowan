class browsing
copyClassNamed: className to: newName

	"anser the new copy of the class"

	| projectSetDefinition |
	projectSetDefinition := self _copyClassDefinitionNamed: className to: newName.

	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	^ Rowan globalNamed: newName