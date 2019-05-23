support - classExtensions definitions
classExtionsClassDefinitions2: packageName className: className in: classDefinitionDictionary

	"Class extension methods"

	^ classDefinitionDictionary
		at: className
			put:
				(RwClassExtensionDefinition
						withProperties: (self propertiesForClassExtensionNamed: className)
						classMethodDefinitions: Dictionary new
						instanceMethodDefinitions:
							(self classExtentionsInstanceMethodsForClass2: className inPackage: packageName));
		yourself