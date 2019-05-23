load project by name
loadProjectNamed: projectNamed

	"load the named project from disk, mark the loaded projects not dirty and run initializers"

	^ self loadProjectNamed: projectNamed instanceMigrator: Rowan platform instanceMigrator