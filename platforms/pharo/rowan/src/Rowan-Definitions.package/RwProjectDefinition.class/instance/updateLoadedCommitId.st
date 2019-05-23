loading
updateLoadedCommitId

	| loadedCommitId |
	loadedCommitId := [  Rowan gitTools gitcommitShaIn: self repositoryRoot pathString ]
		on: Error
		do: [ :ignored | 
			"most likely no commits yet"
			^ nil ].
	self specification imageSpec loadedCommitId: loadedCommitId