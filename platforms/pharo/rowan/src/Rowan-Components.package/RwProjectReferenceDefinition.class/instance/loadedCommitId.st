accessing
loadedCommitId

	^ self properties at: 'loadedCommitId' ifAbsent: [ '' ]