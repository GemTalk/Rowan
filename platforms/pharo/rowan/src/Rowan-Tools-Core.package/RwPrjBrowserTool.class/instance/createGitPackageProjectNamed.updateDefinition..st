definition updating
createGitPackageProjectNamed: projectName updateDefinition: updateBlock

	| projectDefinition |
	projectDefinition := RwProjectDefinition
		newForGitBasedProjectNamed: projectName.
	updateBlock cull: projectDefinition.
	self class load loadProjectDefinition: projectDefinition
