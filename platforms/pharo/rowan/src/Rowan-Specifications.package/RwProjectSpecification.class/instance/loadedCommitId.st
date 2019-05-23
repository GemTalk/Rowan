accessing
loadedCommitId

	imageSpec ifNil: [ ^ '' ].
	^ self imageSpec loadedCommitId
