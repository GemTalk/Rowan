"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: tests
"
testRemovingRepositoryFromPackage

	| old repo repoDigest newClass |
	self testAttachingNewCypressRepositoryToUnknownImagePackages.
	old := currentManager packageInformationNamed: 'X-Y'.
	repo := old repositories any.
	self
		assert: old repositories size = 1
			description: 'There should have been only one repository for the package'.

	old removeRepository: repo.

	self
		assert: old repositories isEmpty
			description: 'There sole repository should have been removed from the package';
		assert: old repositoryDigests isEmpty
			description: 'After removing the last repository, there should not be any repository digests left';
		assert: old digests size = 1
			description: 'After removing the last repository, there should only be the image digest left';
		assert: old digests keys asSet
			equals: (Set with: #IMAGE);
		assert: old imageDigest isNil
			description: 'There should have been no contents in the image'