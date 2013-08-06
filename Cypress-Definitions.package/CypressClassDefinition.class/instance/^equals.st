"
notice: This work is protected by copyright. All rights reserved.
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
