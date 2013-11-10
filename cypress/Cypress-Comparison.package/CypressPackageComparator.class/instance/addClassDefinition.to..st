"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: comparing - private
"
addClassDefinition: classDefinition to: aDictionary

	(aDictionary at: classDefinition className ifAbsentPut: [Dictionary new])
		at: 'class category' put: classDefinition category;
		at: 'class comment' put: classDefinition comment;
		at: 'class definition' put: classDefinition classDefinitionString.
