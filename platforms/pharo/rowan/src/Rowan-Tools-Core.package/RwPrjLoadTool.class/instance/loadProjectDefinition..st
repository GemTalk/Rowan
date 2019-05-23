load project definitions
loadProjectDefinition: projectDefinition
	| projectSetDefinition |
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	^ self loadProjectSetDefinition: projectSetDefinition