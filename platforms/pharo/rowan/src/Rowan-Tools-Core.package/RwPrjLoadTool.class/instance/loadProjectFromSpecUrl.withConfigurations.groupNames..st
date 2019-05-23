load project by spec url
loadProjectFromSpecUrl: fileUrlOrString withConfigurations: configNames groupNames: groupNames

	| projectDefinition projectSetDefinition res |
	projectDefinition := Rowan projectTools create createProjectDefinitionFromSpecUrl: fileUrlOrString.
	projectSetDefinition := Rowan projectTools read 
		readProjectSetForProjectDefinition: projectDefinition 
		withConfigurations: configNames
		groupNames: groupNames.
	res := self loadProjectSetDefinition: projectSetDefinition.
	"loaded project and loaded packages read from disk - mark them not dirty"
	self markProjectSetNotDirty: projectSetDefinition.
	^ res