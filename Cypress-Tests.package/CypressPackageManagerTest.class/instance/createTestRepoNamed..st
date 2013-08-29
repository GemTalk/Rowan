private
createTestRepoNamed: aString

	| repo |
	repo := currentManager
				createRepositoryNamed: aString
				under: '/tmp'
				alias: aString
				schema: 'cypress:'.
	self repositoryToCleanUp: repo.
	^repo
