"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
classDefinitionWithProperties: classPropertiesDict comment: commentString

	| definition properties |
	definition := CypClassDefinition new.
	properties := classPropertiesDict copy.
	properties at: 'comment' put: commentString.
	definition properties: properties.
	^definition