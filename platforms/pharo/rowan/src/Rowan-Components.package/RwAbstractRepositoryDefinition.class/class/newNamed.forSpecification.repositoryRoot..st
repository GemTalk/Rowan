instance creation
newNamed: repositoryName forSpecification: aRwSpecification repositoryRoot: repoRoot

	^ self
		newNamed: repositoryName 
			repositoryRoot: repoRoot
			projectUrl: aRwSpecification projectUrl
			committish: aRwSpecification repoSpec committish 
			committishType: aRwSpecification repoSpec committishType