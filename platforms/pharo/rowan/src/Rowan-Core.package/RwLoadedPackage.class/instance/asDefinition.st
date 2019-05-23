definitions
asDefinition
	"Create a Definition corresponding to what I define in the image."

	^RwPackageDefinition
		withProperties: self propertiesForDefinition
		classDefinitions: self classDefinitions
		classExtensions: self classExtensionDefinitions