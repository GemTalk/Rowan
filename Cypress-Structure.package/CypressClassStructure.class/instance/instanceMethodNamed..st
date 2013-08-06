"
notice: This work is protected by copyright. All rights reserved.
category: querying
"
instanceMethodNamed: methodName

	^self instanceMethods
		at: methodName 
		ifAbsentPut: [CypressMethodStructure named: methodName]
