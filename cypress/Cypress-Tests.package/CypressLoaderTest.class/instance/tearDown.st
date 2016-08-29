"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: running
"
tearDown

	| name |
	super tearDown.
	name := 'Cypress-Mocks'.
	(CypressSnapshot definitions: self baseDefinitions)
		 updatePackage: (CypressPackageDefinition named: name)
