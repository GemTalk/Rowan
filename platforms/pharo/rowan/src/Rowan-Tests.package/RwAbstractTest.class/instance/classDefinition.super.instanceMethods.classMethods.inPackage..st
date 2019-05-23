support
classDefinition: className super: superClassName instanceMethods: instanceMethods classMethods: classMethods inPackage: packageName

	^ Dictionary new
		at: className
			put:
				(RwClassDefinition
						withProperties:
							(self
								propertiesForClassNamed: className
								super: superClassName
								inPackage: packageName)
						classMethodDefinitions: classMethods
						instanceMethodDefinitions: instanceMethods);
		yourself