Class {
	#name : 'RwPrjWriteToolV2',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools-CoreV2'
}

{ #category : 'write' }
RwPrjWriteToolV2 >> writeProjectDefinition: projectDefinition [

	Rowan projectTools createV2 createProjectRepository: projectDefinition.
	projectDefinition 
		exportProjects;
		exportComponents;
		exportPackages
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
