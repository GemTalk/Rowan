accessing
repositoryRoot: aFileReferenceOrPath
	"Root directory of the project. The configsPath, repoPath, specsPath, and projectsPath are specified relative to the repository root."

	| fileRef |
	fileRef := aFileReferenceOrPath asFileReference.
	self projectHome: fileRef parent.
	self projectAlias: fileRef basename