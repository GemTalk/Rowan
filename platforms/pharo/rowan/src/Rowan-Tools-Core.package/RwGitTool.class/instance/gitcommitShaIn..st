smalltalk api
gitcommitShaIn: gitRepoPath

	| result |
	gitRepoPath ifNil: [ ^ '' ].
	result := self gitlogIn: gitRepoPath with: ' -1 --pretty=format:%h '.
	(result beginsWith: 'fatal:')
		ifTrue: [ ^ nil ].
	^ result trimWhiteSpace