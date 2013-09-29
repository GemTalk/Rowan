"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: tests
"
testKnowingPackagesFromExistingRepository
	"The repository used here is not viable for the long run.
	 It is being used to defer creating the tooling to create a directory with
	 controlled contents, instead of creating the needed functionality."

	| repo groups packageNames old new |
	currentManager := self createManagerFromImage.
	groups := self currentPackageInformationGroups.
	repo := currentManager 
		repositoryOn: 'cypressfiletree:/export/galbadia1/users/rsargent/git/gitRepos/' asUrl
		alias: 'Test Repo - Cypress format'.
	packageNames := repo packageNames.
	self assert: packageNames asSet equals: #('Gofer-Core' 'Gofer-Tests' 'NetworkTests' 'Network-Url') asSet.
	old := (packageNames select: [:each | each beginsWith: 'Network'])
		collect: [:each | currentManager packageInformationNamed: each].
	self
		assert: currentManager knownRepositories size = 1
			description: 'After defining a repository, there should be at least one known';
		assert: (currentManager knownRepositories keys collect: [:each | each printString])
			equals: (Set with: 'cypressfiletree:///export/galbadia1/users/rsargent/git/gitRepos/');
		assert: groups size equals: 1;
		assert: (groups at: CypressUnknownPackageInformation) notEmpty
			description: 'There should only beUnknown Package Information objects';
		assert: old size = 2
			description: 'There should be two "Network" packages already in the image';
		assert: (old allSatisfy: [:each | each repositories isEmpty])
			description: 'The packages in the image should still be unknown without a repository'.

	currentManager lookForPackagesInRepository: repo.
	new := packageNames collect: [:each | currentManager packageInformationNamed: each].
	old := old collect: [:each | currentManager packageInformationNamed: each name].
	groups := self currentPackageInformationGroups.

	self
		assert: new size = 4
			description: 'There should be four Package Information objects that were updated';
		assert: groups size equals: 4;
		assert: (groups at: CypressUnknownPackageInformation) notEmpty
			description: 'There should still be some Package Information objects left as Unknown';
		assert: (groups at: CypressKnownPackageInformation) size = new size
			description: 'There should have been one Known Package Information per package in the repository';
		assert: (groups at: CypressKnownPackageInformation) asSet
			equals: new asSet;
		assert: (old allSatisfy: [:each | each imageDigest notNil])
			description: 'Existing packages should have an image digest after connecting to a repository for them'
