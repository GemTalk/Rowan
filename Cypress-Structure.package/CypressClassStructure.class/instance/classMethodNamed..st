"
notice: This work is protected by copyright. All rights reserved.
category: querying
"
classMethodNamed: methodName

	^self classMethods
		at: methodName
		ifAbsentPut: [CypressMethodStructure named: methodName]
