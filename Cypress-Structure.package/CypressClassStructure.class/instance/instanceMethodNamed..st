querying
instanceMethodNamed: methodName

	^self instanceMethods
		at: methodName 
		ifAbsentPut: [CypressMethodStructure named: methodName]
