Class {
	#name : 'RwPrjWriteToolV2',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools-CoreV2'
}

{ #category : 'write' }
RwPrjWriteToolV2 >> writeProjectNamed: projectName [

	| loadedProject projectDefinition |
	loadedProject := Rowan image loadedProjectNamed: projectName.
	projectDefinition := loadedProject asDefinition.
	self writeResolvedProject: projectDefinition.
	"loaded project and loaded packages written to disk - mark them not dirty"
	loadedProject markNotDirty.
	loadedProject loadedPackages valuesDo: [:loadedPackage | loadedPackage markNotDirty ].
]

{ #category : 'write' }
RwPrjWriteToolV2 >> writeResolvedProject: resolvedProject [
	"write project artifacts, EXCEPT load spec as the load spec is typically modified during load.
		The load spec should be written shortly after being read from disk AFTER any
		intentional changes have been made"

	Rowan projectTools createV2
		createResolvedProjectRepository: resolvedProject repository.
	resolvedProject
		exportProjectSpecification;
		exportProjects;
		exportComponents;
		exportPackages
]
