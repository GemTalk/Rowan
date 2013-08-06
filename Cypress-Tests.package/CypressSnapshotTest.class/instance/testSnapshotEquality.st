"
notice: This work is protected by copyright. All rights reserved.
category: tests
"
testSnapshotEquality
	| name pkg packageDefinitions expectedDefinitions |
	name := 'Cypress-Mocks'.
	pkg := CypressPackageDefinition named: name.
	packageDefinitions := pkg snapshot definitions.
	expectedDefinitions := self baseDefinitions.
	self assert: packageDefinitions asArray equals: expectedDefinitions asArray
