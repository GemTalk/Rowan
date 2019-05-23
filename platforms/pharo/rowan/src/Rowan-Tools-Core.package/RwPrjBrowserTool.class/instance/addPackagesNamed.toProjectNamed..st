package browsing
addPackagesNamed: packageNames toProjectNamed: projectName

	| projectDefinition |
	projectDefinition := self _projectNamed: projectName.
	packageNames
		do: [ :packageName | projectDefinition addPackage: (RwPackageDefinition newNamed: packageName) ].
	Rowan projectTools load loadProjectDefinition: projectDefinition