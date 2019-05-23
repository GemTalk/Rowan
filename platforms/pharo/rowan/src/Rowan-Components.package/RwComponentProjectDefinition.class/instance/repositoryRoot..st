properties
repositoryRoot: aFileReferenceOrPath
	"Root directory of the project. The configsPath, repoPath, specsPath, and projectsPath are specified relative to the repository root."

	^ self projectRef repositoryRoot: aFileReferenceOrPath