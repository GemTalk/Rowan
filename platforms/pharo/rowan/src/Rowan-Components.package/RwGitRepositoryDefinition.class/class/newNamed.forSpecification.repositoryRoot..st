instance creation
newNamed: repositoryName forSpecification: aRwSpecification repositoryRoot: repoRoot

	^ (super newNamed: repositoryName forSpecification: aRwSpecification repositoryRoot: repoRoot)
		committish: aRwSpecification repoSpec committish committishType: aRwSpecification repoSpec committishType;
		remoteUrl: aRwSpecification remoteUrl;
		yourself