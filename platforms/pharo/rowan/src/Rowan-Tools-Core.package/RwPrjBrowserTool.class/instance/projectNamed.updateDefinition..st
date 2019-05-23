definition updating
projectNamed: projectName updateDefinition: updateBlock

	| projectDefinition |
	projectDefinition := self _projectNamed: projectName.
	updateBlock value: projectDefinition.
	self class load loadProjectDefinition: projectDefinition