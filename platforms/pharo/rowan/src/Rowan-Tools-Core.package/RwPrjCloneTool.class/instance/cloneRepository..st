smalltalk api
cloneRepository: aRwGitRepositoryDefinition

	| gitTool response remoteUrl cloneOption checkout |

	self
		_validateForGitRepository: aRwGitRepositoryDefinition
		ifDone: [ :msg | 
			"done block is invoked when it is not necessary to perform further processing. Typically,
		the clone is already present."
			^ msg ].
	gitTool := Rowan gitTools.
	remoteUrl := aRwGitRepositoryDefinition remoteUrl , ' '.
	cloneOption := ' --no-checkout '.
	checkout := aRwGitRepositoryDefinition committish.
	checkout ifNil: [ cloneOption := '' ].
	response := gitTool gitcloneIn: aRwGitRepositoryDefinition repositoryRoot parent pathString with: cloneOption , remoteUrl, aRwGitRepositoryDefinition repositoryRoot pathString.
	checkout
		ifNotNil: [ Rowan projectTools checkout checkoutProjectReference: aRwGitRepositoryDefinition ].