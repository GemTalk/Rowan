"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: querying
"
instanceMethodNamed: methodName

	^self instanceMethods
		at: methodName 
		ifAbsentPut: [CypressMethodStructure named: methodName]
