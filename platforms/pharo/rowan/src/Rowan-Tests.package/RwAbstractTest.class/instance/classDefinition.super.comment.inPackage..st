support
classDefinition: className super: superClassName comment: comment inPackage: packageName

	^ Dictionary new
		at: className
			put:
				(RwClassDefinition
						withProperties:
							(self
								propertiesForClassNamed: className
								super: superClassName
								comment: comment
								inPackage: packageName)
						classMethodDefinitions: Dictionary new
						instanceMethodDefinitions: Dictionary new);
		yourself