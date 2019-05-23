load project by name
loadProjectNamed: projectNamed withConfigurations: configNames groupNames: groupNames instanceMigrator: instanceMigrator

	"load the named project from disk, mark the loaded projects not dirty and run initializers"

	| projectSetDefinition res |
	projectSetDefinition := self class read
		readProjectSetForProjectNamed: projectNamed
		withConfigurations: configNames 
		groupNames: groupNames.
	res := self 
		_doProjectSetLoad_254: projectSetDefinition
		instanceMigrator: instanceMigrator 
		originalProjectSet: projectSetDefinition 
		processedClassNames: Set new.
	"loaded project and loaded packages read from disk - mark them not dirty"
	self markProjectSetNotDirty: projectSetDefinition.
	^ res
