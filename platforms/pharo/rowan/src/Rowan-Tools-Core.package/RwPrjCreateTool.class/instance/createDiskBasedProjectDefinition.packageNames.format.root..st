project definition creation
createDiskBasedProjectDefinition: projectName packageNames: packageNames format: format root: rootPath
	"Create a new project definition for a disk-based Rowan project with the given attributes"

	| utils rootRepoPath |
	utils := self fileUtilities.
	rootRepoPath := rootPath , utils pathNameDelimiter , projectName.
	^ (RwProjectDefinition newForDiskBasedProjectNamed: projectName)
		packageNames: packageNames;
		repositoryRootPath: rootRepoPath;
		projectOwnerId: Rowan image currentUserId;
		yourself.
