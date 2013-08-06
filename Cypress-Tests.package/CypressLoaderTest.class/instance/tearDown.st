"
notice: This work is protected by copyright. All rights reserved.
category: running
"
tearDown

	| name |
	super tearDown.
	name := 'Cypress-Mocks'.
	(CypressSnapshot definitions: self baseDefinitions)
		 updatePackage: (CypressPackageDefinition named: name)
