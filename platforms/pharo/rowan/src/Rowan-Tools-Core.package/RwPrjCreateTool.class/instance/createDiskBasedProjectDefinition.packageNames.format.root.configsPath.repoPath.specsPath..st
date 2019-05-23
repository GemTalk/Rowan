project definition creation
createDiskBasedProjectDefinition: projectName packageNames: packageNames format: format root: rootPath configsPath: configsPath repoPath: repoPath specsPath: specsPath
	"Create a new project definition for a disk-based Rowan project with the given attributes"

	| utils rootRepoPath |
	utils := self fileUtilities.
	rootRepoPath := rootPath , utils pathNameDelimiter , projectName.
	^ (RwProjectDefinition newForDiskBasedProjectNamed: projectName)
		packageNames: packageNames;
		configsPath: configsPath;
		repoPath: repoPath;
		specsPath: specsPath;
		repositoryRootPath: rootRepoPath;
		projectOwnerId: Rowan image currentUserId;
		yourself.
