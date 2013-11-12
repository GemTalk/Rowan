"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
createFileTreeReadOnlyTestRepoNamed: aString

	| repo |
	repo := currentManager
				createRepositoryNamed: aString
				under: '/tmp'
				alias: aString
				schema: 'cypressfiletree:'.
	self repositoryToCleanUp: repo.
	^repo
