tests
testExtensionsOnlySnapshot

	| name pkg packageDefinitions expectedDefinitions |
	name := 'Cypress-Mocks-Extensions'.
	pkg := CypressPackageDefinition named: name.
	packageDefinitions := pkg snapshot definitions.
	self
		assert: packageDefinitions size equals: 1;
		assert: packageDefinitions first selector equals: 'isCypressMockBasic';
		assert: packageDefinitions first className equals: 'Object'