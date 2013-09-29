"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: tests
"
testPrintString
	| name pkg |
	name := 'Cypress-Mocks'.
	pkg := CypressPackageDefinition named: name.
	self assert: 'a CypressPackageDefinition(', name, ')' equals: pkg printString.
