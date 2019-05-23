accessing
gitRoot: aGitRootReferenceOrString

	aGitRootReferenceOrString ifNil: [ ^ self properties removeKey: 'gitRoot' ifAbsent: [] ].
	^ self properties at: 'gitRoot' put: aGitRootReferenceOrString asFileReference