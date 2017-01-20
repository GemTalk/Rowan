"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: definitions
"
asDefinition
	"Create a Definition corresponding to what I define in the image."
	^CypClassDefinition
		withProperties: self propertiesForDefinition
		classMethodDefinitions: self classMethodDefinitions
		instanceMethodDefinitions: self instanceMethodDefinitions