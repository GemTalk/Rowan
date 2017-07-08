"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: comparing - private
"
add: aDefinition to: aDictionary

	aDefinition
		classDefinition: [:classDefinition | self addClassDefinition: classDefinition to: aDictionary]
		methodDefinition: [:methodDefinition | self addMethodDefinition: methodDefinition to: aDictionary]
