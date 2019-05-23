smalltalk api
writeComponentProjectDefinition: projectDefinition

	Rowan projectTools create createProjectRepository: projectDefinition projectRef.
	projectDefinition 
		exportProjects;
		exportComponents;
		exportPackages