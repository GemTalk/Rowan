tests
testLoaderWithUnloadable

	| name loader |
	name := 'Cypress-Mocks'.
	self
		should: 
			[(CypressSnapshot
				definitions: self unloadableDefinitions , self targetDefinitions)
					updatePackage: (CypressPackageDefinition named: name)]
		raise: CypressLoaderMissingClasses
		description: 'There should have been some unloadable definitions'.
	loader := 
			[(CypressSnapshot
				definitions: self unloadableDefinitions , self targetDefinitions)
					updatePackage: (CypressPackageDefinition named: name)]
					on: CypressLoaderMissingClasses
					do: 
						[:ex |
						self
							assert: #('AnotherNonExistentClass' 'NonExistentClass' 'UnloadableClass')
							equals: ex requirementsMap keys asSortedCollection asArray.
						ex resume].
	self
		assert: loader additions size equals: 5;
		assert: loader removals size equals: 2;
		assert: loader unloadable size equals: 3;
		assert: (loader unloadable gather: [:each | each]) size equals: 3;
		assert: loader provisions notEmpty
			description: 'There should have been a number of classes provided by the system';
		assert: loader errors size equals: 0;
		assert: loader methodAdditions size equals: 0;
		assert: loader requirements size equals: 2