Class {
	#name : 'RwPrjDisownTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'projects' }
RwPrjDisownTool >> disownProjectNamed: projectName [

	"Remove the classes and methods managed by the project from the loaded things."

	"The classes and methods are not removed from the system"

	"Use the adopt tool to restore the classes and methods to the loaded things"

	| loadedProject disownTool |

	loadedProject := Rowan image loadedProjectNamed: projectName.

	disownTool := Rowan packageTools disown.
	loadedProject loadedPackages 
		do: [:loadedPackage |
			disownTool disownPackageNamed: loadedPackage name ].

	Rowan image _removeLoadedProject: loadedProject

]
