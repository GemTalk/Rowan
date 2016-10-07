"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: tests
"
testExtensionsOnlySnapshot

	| name pkg packageDefinitions |
	name := 'Cypress-Mocks-Extensions'.
	pkg := CypressPackageDefinition named: name.
	packageDefinitions := pkg snapshot definitions.
	self
		assert: packageDefinitions size equals: 1;
		assert: packageDefinitions first selector equals: 'isCypressMockBasic';
		assert: packageDefinitions first className equals: 'Object'