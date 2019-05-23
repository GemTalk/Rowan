accessing
repositoryCommitId

	^ [  Rowan gitTools gitcommitShaIn: self gitRoot pathString ]
			on: Error
			do: [ :ignored | 
			"most likely no commits yet"
			'' ]