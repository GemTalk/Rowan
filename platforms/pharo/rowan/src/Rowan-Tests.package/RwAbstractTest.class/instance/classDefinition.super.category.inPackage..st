support
classDefinition: className super: superClassName category: category inPackage: packageName

	^ Dictionary new
		at: className
			put:
				(RwClassDefinition
						withProperties:
							(self
								propertiesForClassNamed: className
								super: superClassName
								category: category
								inPackage: packageName)
						classMethodDefinitions: Dictionary new
						instanceMethodDefinitions: Dictionary new);
		yourself