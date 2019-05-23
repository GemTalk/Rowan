smalltalk api
adoptProjectSetDefinition: projectSetDefinitionToAdopt

	projectSetDefinitionToAdopt projects do: [:projectDef |
		self adoptProjectDefinition: projectDef ]
