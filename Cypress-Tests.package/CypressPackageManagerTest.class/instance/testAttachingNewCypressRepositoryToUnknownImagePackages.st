tests
testAttachingNewCypressRepositoryToUnknownImagePackages

	| repo groups old new |
	currentManager := self createManagerWithUnknownPackages: #('X' 'X-Y' 'X-Y-A' 'X-Y-B' 'X-Y-C' 'X-Z').
	groups := self currentPackageInformationGroups.
	self
		assert: currentManager knownRepositories isEmpty
			description: 'Newly created Package Manager should not know about any repositories';
		assert: currentManager packageInformationList size equals: 6;
		assert: groups size equals: 1;
		assert: groups keys asArray first equals: CypressUnknownPackageInformation.
	
	repo := self createTestRepoNamed: 'CypressTestRepo'.
	old := currentManager packageInformationNamed: 'X-Y'.
	self deny: old isKnown description: 'Newly defined packages are supposed to be Unknown'.

	currentManager assignRepository: repo to: old.
	new := currentManager packageInformationNamed: 'X-Y'.
	groups := self currentPackageInformationGroups.

	self
		assert: currentManager knownRepositories size = 1
			description: 'After assigning a repository, there should be at least one known';
		assert: (currentManager knownRepositories keys collect: [:each | each printString])
			equals: (Set with: 'cypress:///tmp/CypressTestRepo/');
		assert: currentManager packageInformationList size = 6
			description: 'There should be the same number of Package Information objects';
		assert: groups size equals: 4;
		assert: (groups at: CypressUnknownPackageInformation) size = 1
			description: 'There should have been one Package Information left as Unknown';
		assert: (groups at: CypressUnknownPackageInformation) first name
			equals: 'X-Z';
		assert: (groups at: CypressKnownPackageInformation) size = 1
			description: 'There should have been one Package Information left as Known';
		assert: (groups at: CypressKnownPackageInformation) first name
			equals: 'X-Y';
		assert: (groups at: CypressEclipsedPackageInformation) size = 3
			description: 'There should have been one Package Information left as Eclipsed';
		assert: ((groups at: CypressEclipsedPackageInformation)
					collect: [:each | each name]) asSet
			equals: (Set with: 'X-Y-A' with: 'X-Y-B' with: 'X-Y-C');
		assert: (groups at: CypressConflictingPackageInformation) size = 1
			description: 'There should have been one Package Information left as Conflicting';
		assert: (groups at: CypressConflictingPackageInformation) first name
			equals: 'X';
		assert: new repositories size = 1
			description: 'After assigning a repository to a newly known package, it should have one repository';
		assert: new repositories any 
			equals: repo;
		assert: (new digestFor: repo url)
			equals: (CypressPackageStructure named: new name, '.package') digest
