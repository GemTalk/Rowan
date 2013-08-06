tests
testPackageStructureFromJson

	| packageStructure classStructure classProperties |
	packageStructure := CypressPackageStructure fromJson: self basePackageStructureJson.
	self assert: packageStructure name equals: 'Cypress-Mocks.package'.
	self assert: packageStructure packageName equals: 'Cypress-Mocks'.
	self assert: packageStructure properties isEmpty description: 'Properties should have been empty'.
	self assert: packageStructure extensions size equals: 1.
	self assert: packageStructure classes size equals: 1.
