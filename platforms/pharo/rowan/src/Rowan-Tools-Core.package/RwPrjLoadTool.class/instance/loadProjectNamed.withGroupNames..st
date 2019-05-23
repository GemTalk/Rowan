load project by name
loadProjectNamed: projectNamed withGroupNames: groupNames

	"load the named project from disk, mark the loaded projects not dirty and run initializers"

	^ self loadProjectNamed: projectNamed withGroupNames: groupNames instanceMigrator: Rowan platform instanceMigrator
