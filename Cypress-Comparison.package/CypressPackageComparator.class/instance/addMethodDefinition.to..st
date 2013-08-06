"
notice: This work is protected by copyright. All rights reserved.
category: comparing - private
"
addMethodDefinition: methodDefinition to: aDictionary

	((aDictionary at: methodDefinition className ifAbsentPut: [Dictionary new])
		at: (methodDefinition classIsMeta
				ifTrue: ['class methods']
				ifFalse: ['instance methods'])
		ifAbsentPut: [Dictionary new]) at: methodDefinition selector
			put: methodDefinition category -> methodDefinition source