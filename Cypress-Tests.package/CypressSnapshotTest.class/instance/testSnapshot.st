"
notice: This work is protected by copyright. All rights reserved.
category: tests
"
testSnapshot

	| name pkg packageDefinitions expectedDefinitions |
	name := 'Cypress-Mocks'.
	pkg := CypressPackageDefinition named: name.
	packageDefinitions := pkg snapshot definitions.
	expectedDefinitions := self baseDefinitions.
	self assert: expectedDefinitions size equals: packageDefinitions size.
	packageDefinitions
		do: [:def | self assert: (expectedDefinitions includes: def)]