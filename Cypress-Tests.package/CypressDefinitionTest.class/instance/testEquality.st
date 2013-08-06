"
notice: This work is protected by copyright. All rights reserved.
category: tests
"
testEquality
	| pkg1 pkg2 pkg3 name |
	name := 'Cypress-Mocks'.
	pkg1 := CypressPackageDefinition named: name.
	pkg2 := CypressPackageDefinition named: name.
	pkg3 := CypressPackageDefinition named: 'Nope!'.

	self assert: pkg1 equals: pkg2.
	self deny: pkg1 equals: pkg3
