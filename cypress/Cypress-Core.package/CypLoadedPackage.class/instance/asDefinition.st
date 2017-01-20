"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: definitions
"
asDefinition
	"Create a Definition corresponding to what I define in the image."

	^CypPackageDefinition
		withProperties: self propertiesForDefinition
		classDefinitions: self classDefinitions
		classExtensions: self classExtensionDefinitions