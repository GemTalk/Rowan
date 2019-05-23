support
classDefinition: className super: superClassName classVars: classVars inPackage: packageName

	^ Dictionary new
		at: className
			put:
				(RwClassDefinition
						withProperties:
							(self
								propertiesForClassNamed: className
								super: superClassName
								classVars: classVars
								inPackage: packageName)
						classMethodDefinitions: Dictionary new
						instanceMethodDefinitions: Dictionary new);
		yourself