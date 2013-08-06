tests
testLoaderWithClassDefinitionError

	| name loader |
	name := 'Cypress-Mocks'.
	self
		should: 
			[(CypressSnapshot
				definitions: self erroneousClassDefinitions , self targetDefinitions)
					updatePackage: (CypressPackageDefinition named: name)]
		raise: CypressLoaderErrorNotification
		description: 'There should have been a class definition with an error'.
	loader := 
			[(CypressSnapshot
				definitions: self erroneousClassDefinitions , self targetDefinitions)
					updatePackage: (CypressPackageDefinition named: name)]
					on: CypressLoaderError
					do: 
						[:ex |
						self
							assert: ex patchOperation definition name
							equals: 'CypressMockBasicSubclassWithDuplicateInstanceVariable'.
						ex resume].
	self
		assert: loader additions size equals: 4;
		assert: loader removals size equals: 2;
		assert: loader unloadable size equals: 0;
		assert: (loader unloadable gather: [:each | each]) size equals: 0;
		assert: loader provisions notEmpty
			description: 'There should have been a number of classes provided by the system';
		assert: loader errors size equals: 1;
		assert: loader methodAdditions size equals: 0;
		assert: loader requirements size equals: 0