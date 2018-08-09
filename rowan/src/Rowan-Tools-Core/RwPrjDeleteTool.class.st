Class {
	#name : 'RwPrjDeleteTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'delete project definitions' }
RwPrjDeleteTool >> deleteProjectDefinition: projectDefinition [

	"Remove the classes and methods managed by the project definition."

	"The classes and methods are removed from the system"

	"If there are subclasses of classes in other projects the delete will fail ... deleteProjectDefinitionSet: to delete multiple projects"

	| projectSetDefinition |
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.

	^ self deleteProjectSetDefinition: projectSetDefinition

]

{ #category : 'delete projects by name' }
RwPrjDeleteTool >> deleteProjectNamed: projectName [

	"Remove the classes and methods managed by the project from the loaded things."

	"The classes and methods are removed from the system"

	"If there are subclasses of classes in other projects the delete will fail ... deleteProjectDefinitionSet: to delete multiple projects"

	| loadedProject |

	loadedProject := Rowan image loadedProjectNamed: projectName.

	^ self deleteProjectDefinition: loadedProject asDefinition

]

{ #category : 'delete project definitions' }
RwPrjDeleteTool >> deleteProjectSetDefinition: projectSetDefinitionToDelete [

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

]
