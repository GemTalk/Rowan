"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: tests
"
testInitialPopulationOfPackageInformation
	"A clean ab initio start up will only have Unknown packages
	 (or potential packages) based on the categories used in the image."

	currentManager := self createManagerFromImage.
	self
		assert: currentManager knownRepositories isEmpty
			description: 'Newly created Package Manager should not know about any repositories';
		assert: currentManager packageInformationList notEmpty
			description: 'It is impossible for an image to have no categorized classes';
		assert: (currentManager packageInformationList collect: [:each | each class]) asSet
			equals: (Set with: CypressUnknownPackageInformation)