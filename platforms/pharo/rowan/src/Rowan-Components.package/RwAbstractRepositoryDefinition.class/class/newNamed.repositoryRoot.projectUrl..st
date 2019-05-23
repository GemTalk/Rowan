instance creation
newNamed: repositoryName repositoryRoot: repoRoot projectUrl: anUrlString

	^ self new
		name: repositoryName;
		repositoryRoot: repoRoot;
		projectUrl: anUrlString;
		yourself