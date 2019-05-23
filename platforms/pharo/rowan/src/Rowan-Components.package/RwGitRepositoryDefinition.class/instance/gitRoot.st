accessing
gitRoot
	"The root directory of the git repository that the project is located in. If the project is not git based
		or the git root is not explicitly assigned, git root is synonymous with repository root."

	^ self properties at: 'gitRoot' ifAbsent: [ self repositoryRoot ]