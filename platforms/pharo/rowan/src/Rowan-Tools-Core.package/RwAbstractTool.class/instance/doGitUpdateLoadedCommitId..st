git
doGitUpdateLoadedCommitId: gitRootPath

	| gitTool loadedCommitId |
	gitTool := Rowan gitTools.
	loadedCommitId := [ gitTool gitcommitShaIn: gitRootPath asFileReference pathString ]
		on: Error
		do: [ :ignored | 
			"most likely no commits yet"
			^ nil ].
	^ specification imageSpec loadedCommitId: loadedCommitId