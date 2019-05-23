delete project definitions
deleteProjectSetDefinition: projectSetDefinitionToDelete

	"Remove the classes and methods managed by the listed project definitions."

	"The classes and methods are removed from the system"

	"If there are subclasses of classes in other projects not in the set, the delete will fail ... 
		add projects containing  subclasses to the <projectSetDefinitionToDelete> ."

	| loadedProjectSet |
	loadedProjectSet := projectSetDefinitionToDelete deriveLoadedThings.

	projectSetDefinitionToDelete definitions
		do: [ :projectDef |
			projectDef packageNames do: [:packageName | 
				projectDef removePackageNamed: packageName ] ].
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinitionToDelete.

	loadedProjectSet do: [:loadedProject |
		Rowan image _removeLoadedProject: loadedProject ]
