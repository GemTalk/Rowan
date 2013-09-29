"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
createFileTreeCompatibleTestRepoNamed: aString

	| repo |
	repo := currentManager
				createRepositoryNamed: aString
				under: '/tmp'
				alias: aString
				schema: 'cypressft:'.
	self repositoryToCleanUp: repo.
	^repo
