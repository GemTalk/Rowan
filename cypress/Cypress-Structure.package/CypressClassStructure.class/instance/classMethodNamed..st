"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: querying
"
classMethodNamed: methodName

	^self classMethods
		at: methodName
		ifAbsentPut: [CypressMethodStructure named: methodName]
