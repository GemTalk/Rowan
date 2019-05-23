load project by name
loadProjectNamed: projectNamed withConfigurations: configNames groupNames: groupNames

	"load the named project from disk, mark the loaded projects not dirty and run initializers"

	^ self loadProjectNamed: projectNamed withConfigurations: configNames groupNames: groupNames instanceMigrator: Rowan platform instanceMigrator
