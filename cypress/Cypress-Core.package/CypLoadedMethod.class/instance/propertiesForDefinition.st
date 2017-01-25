"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: definitions
"
propertiesForDefinition

	| props |
	props := super propertiesForDefinition.
	props at: 'selector' put: name.
	^props