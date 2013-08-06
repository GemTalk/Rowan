"
notice: This work is protected by copyright. All rights reserved.
category: tests
"
testNameEquality
	| pkg name |
	name := 'Cypress-Mocks'.
	pkg := CypressPackageDefinition named: name.
	self assert: pkg name equals: name.
	self deny: pkg name equals: 'Nope.'.
