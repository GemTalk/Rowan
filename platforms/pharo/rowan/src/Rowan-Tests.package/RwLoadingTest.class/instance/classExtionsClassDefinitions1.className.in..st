support - classExtensions definitions
classExtionsClassDefinitions1: packageName className: className in: classDefinitionDictionary

	"Class extension methods"

	^ classDefinitionDictionary
		at: className
			put:
				(RwClassDefinition
						withProperties:
							(self
								propertiesForClassNamed: className
								super: 'Object'
								instvars: {}
								classinstvars: {}
								classvars: {}
								comment: 'a comment'
								pools: {}
								inPackage: packageName)
						classMethodDefinitions: Dictionary new
						instanceMethodDefinitions: Dictionary new);
		yourself