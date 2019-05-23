definition updating
projectsNamed: projectNames updateDefinition: updateBlock

	| projectSetDefinition |
	projectSetDefinition := RwProjectSetDefinition new.
	projectNames
		do: [ :projectName | 
			| projectDefinition |
			projectDefinition := self _projectNamed: projectName.
			updateBlock value: projectDefinition.
			projectSetDefinition addProject: projectDefinition ].
	self class load loadProjectSetDefinition: projectSetDefinition