querying
classMethodNamed: methodName

	^self classMethods
		at: methodName
		ifAbsentPut: [CypressMethodStructure named: methodName]
