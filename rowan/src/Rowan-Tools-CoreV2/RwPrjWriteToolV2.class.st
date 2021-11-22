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
	Rowan projectTools createV2
		createResolvedProjectRepository: resolvedProject repository.
	resolvedProject
		exportProjectSpecification;
		exportProjects;
		exportComponents;
		exportPackages
]

{ #category : 'write' }
RwPrjWriteToolV2 >> writeResolvedProjectPharoTonelFormatV1: resolvedProject [
	Rowan projectTools createV2
		createResolvedProjectRepository: resolvedProject repository.
	resolvedProject
		exportProjectSpecification;
		exportProjects;
		exportComponents;
		exportPharoTonelFormatV1Packages
]
