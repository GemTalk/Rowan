package browsing
addPackageNamed: packageName toProjectNamed: projectName

	| projectDefinition |
	projectDefinition := self _projectNamed: projectName.
	projectDefinition addPackage: (RwPackageDefinition newNamed: packageName).
	Rowan projectTools load loadProjectDefinition: projectDefinition