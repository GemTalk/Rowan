instance creation
newNamed: repositoryName repositoryRoot: repoRoot projectUrl: anUrlString committish: committish committishType: committishType

	^ self new
		name: repositoryName;
		repositoryRoot: repoRoot;
		projectUrl: anUrlString;
		committish: committish committishType: committishType;
		yourself