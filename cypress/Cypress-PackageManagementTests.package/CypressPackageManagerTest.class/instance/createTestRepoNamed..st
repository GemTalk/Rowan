"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
createTestRepoNamed: aString

	| repo |
	repo := currentManager
				createRepositoryNamed: aString
				under: '/tmp'
				alias: aString
				schema: 'cypress:'.
	self repositoryToCleanUp: repo.
	^repo
