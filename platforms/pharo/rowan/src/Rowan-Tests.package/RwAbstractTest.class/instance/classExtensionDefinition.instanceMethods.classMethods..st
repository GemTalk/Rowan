support
classExtensionDefinition: className instanceMethods: instanceMethods classMethods: classMethods

	^ Dictionary new
		at: className
			put:
				(RwClassExtensionDefinition
						withProperties: (self propertiesForClassExtensionNamed: className)
						classMethodDefinitions: classMethods
						instanceMethodDefinitions: instanceMethods);
		yourself