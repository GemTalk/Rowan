"
notice: This work is protected by copyright. All rights reserved.
category: tests
"
testPackageStructureFromPackage

	| packageStructure packageDefinitions expectedDefinitions |
	packageStructure := CypressPackageStructure
				fromPackage: (CypressPackageDefinition named: 'Cypress-Mocks').
	packageDefinitions := packageStructure snapshot definitions.
	expectedDefinitions := self baseDefinitions.
	self assert: expectedDefinitions size equals: packageDefinitions size.
	packageDefinitions do: 
			[:def |
			self assert: (expectedDefinitions includes: def)
				description: 'Definition for ', def printString, ' did not match expected ones']