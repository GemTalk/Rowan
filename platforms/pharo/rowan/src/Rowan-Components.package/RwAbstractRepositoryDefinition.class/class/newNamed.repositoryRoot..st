instance creation
newNamed: repositoryName repositoryRoot: repoRoot 

	^ self new
		name: repositoryName;
		repositoryRoot: repoRoot;
		yourself