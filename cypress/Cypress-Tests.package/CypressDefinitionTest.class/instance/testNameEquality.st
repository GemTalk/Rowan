"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: tests
"
testNameEquality
	| pkg name |
	name := 'Cypress-Mocks'.
	pkg := CypressPackageDefinition named: name.
	self assert: pkg name equals: name.
	self deny: pkg name equals: 'Nope.'.
