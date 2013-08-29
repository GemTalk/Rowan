private
createFileTreeCompatibleTestRepoNamed: aString

	| repo |
	repo := currentManager
				createRepositoryNamed: aString
				under: '/tmp'
				alias: aString
				schema: 'cypressft:'.
	self repositoryToCleanUp: repo.
	^repo
