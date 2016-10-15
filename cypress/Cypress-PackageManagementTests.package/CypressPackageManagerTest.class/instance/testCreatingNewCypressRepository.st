"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: tests
"
testCreatingNewCypressRepository

	| repo |
	currentManager := self createManagerFromImage.
	repo := self createTestRepoNamed: 'CypressTestRepo'.

	self
		assert: repo directoryPath
			equals: '/tmp/CypressTestRepo/';
		assert: repo packageNames isEmpty
			description: 'a new repository should be empty of packages';
		assert: repo isCodeFormatCypress
			equals: true;
		assert: repo isCodeFormatStrict
			equals: true
