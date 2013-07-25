tests
testPackageStructureSnapshot

	| packageStructure packageDefinitions expectedDefinitions |
	packageStructure := CypressPackageStructure
				fromJs: (self compileJSON: self basePackageStructureJson).
	packageDefinitions := packageStructure snapshot definitions.
	expectedDefinitions := self baseDefinitions.
	self assert: expectedDefinitions size equals: packageDefinitions size.
	packageDefinitions do: 
			[:def |
			self assert: (expectedDefinitions includes: def)
				description: 'Definition was not as expected']