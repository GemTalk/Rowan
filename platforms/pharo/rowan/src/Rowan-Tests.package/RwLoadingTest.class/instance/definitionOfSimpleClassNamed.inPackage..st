support - simple definitions
definitionOfSimpleClassNamed: name inPackage: packageName

	^RwClassDefinition
		withProperties: (self propertiesForSimpleClassNamed: name inPackage: packageName)
		classMethodDefinitions: (self sillyClassMethodsForClassNamed: name)
		instanceMethodDefinitions: (self accessorMethodsForSimpleClassNamed: name)