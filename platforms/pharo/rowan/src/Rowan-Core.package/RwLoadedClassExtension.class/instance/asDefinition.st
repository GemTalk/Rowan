definitions
asDefinition
	"Create a Definition corresponding to what I define in the image."
	^RwClassExtensionDefinition
		withProperties: self propertiesForDefinition
		classMethodDefinitions: self classMethodDefinitions
		instanceMethodDefinitions: self instanceMethodDefinitions