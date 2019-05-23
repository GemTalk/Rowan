smalltalk api
checkoutProjectReference: aRwGitProjectReference

	| checkout |
	checkout := aRwGitProjectReference  committish.
	checkout
		ifNil: [ 
			| msg |
			msg := 'No committish defined for the project reference '
				, aRwGitProjectReference projectAlias printString.
			self inform: msg.
			^ msg ]
		ifNotNil: [ 
			| gitTool gitRepoPath |
			gitTool := Rowan gitTools.
			gitRepoPath := aRwGitProjectReference repositoryRoot.
			gitTool gitcheckoutIn: gitRepoPath with: checkout ].
	^ aRwGitProjectReference