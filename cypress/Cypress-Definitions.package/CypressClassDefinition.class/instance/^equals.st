"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: comparing
"
= aDefinition
	^(super = aDefinition)
		and: [superclassName = aDefinition superclassName
		and: [category = aDefinition category
		and: [instVarNames = aDefinition instVarNames
		and: [classInstVarNames = aDefinition classInstVarNames
		and: [classVarNames = aDefinition classVarNames
		and: [poolDictionaryNames = aDefinition poolDictionaryNames
		and: [comment = aDefinition comment]]]]]]]
