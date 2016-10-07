"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: tests
"
testLoad

	| name loader |
	name := 'Cypress-Mocks'.
	loader := (CypressSnapshot definitions: self targetDefinitions)
				updatePackage: (CypressPackageDefinition named: name).
	self
		assert: loader additions size equals: 5;
		assert: loader removals size equals: 2;
		assert: loader unloadable size equals: 0;
		assert: loader provisions notEmpty
			description: 'There should have been a number of classes provided by the system';
		assert: loader errors size equals: 0;
		assert: loader methodAdditions size equals: 0;
		assert: loader requirements isEmpty
			description: 'There should have been no external requirements'