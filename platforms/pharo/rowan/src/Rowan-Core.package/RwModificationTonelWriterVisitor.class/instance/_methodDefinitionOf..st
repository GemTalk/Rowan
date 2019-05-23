class writing
_methodDefinitionOf: aMethodDefinition
	^ self _toSTON: (self class orderedDictionaryClass new 
		at: #category put: aMethodDefinition protocol; 
		yourself)