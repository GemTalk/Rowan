"
notice: This work is protected by copyright. All rights reserved.
category: tests
"
testPrintString
	| name pkg |
	name := 'Cypress-Mocks'.
	pkg := CypressPackageDefinition named: name.
	self assert: 'a CypressPackageDefinition(', name, ')' equals: pkg printString.
