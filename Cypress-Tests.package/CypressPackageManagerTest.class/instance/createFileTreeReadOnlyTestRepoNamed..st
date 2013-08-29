private
createFileTreeReadOnlyTestRepoNamed: aString

	| repo |
	repo := currentManager
				createRepositoryNamed: aString
				under: '/tmp'
				alias: aString
				schema: 'cypressfiletree:'.
	self repositoryToCleanUp: repo.
	^repo
