private
_createProjectRepositoryDirectory: repoPathIn useGit: useGit abort: abortBlock

	| gitTool repoPath |
	gitTool := Rowan gitTools.
	repoPath := repoPathIn asFileReference.
	repoPath exists
		ifTrue: [ 
			(self
				confirm:
					'There is already a directory named ' , repoPath pathString printString
						, '. The contents of the directory will be deleted if you continue.')
				== true
				ifFalse: [ ^ abortBlock value ].
			repoPath ensureDeleteAllChildren ]
		ifFalse: [ repoPath ensureCreateDirectory ].
	useGit
		ifTrue: [ 
			(gitTool gitPresentIn: repoPath pathString )
				ifFalse: [ 
					"create a git repository"
					gitTool gitinitIn: repoPath pathString with: '' ] ]