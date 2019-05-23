definitions
asDefinition
	"Create a Definition corresponding to what I define in the image."

	^RwMethodDefinition withProperties: self propertiesForDefinition
		source: self source