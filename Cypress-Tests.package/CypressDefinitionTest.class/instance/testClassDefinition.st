"
notice: This work is protected by copyright. All rights reserved.
category: tests
"
testClassDefinition

	self
		assert: (CypressClassDefinition
					name: 'Foo'
					superclassName: 'Object'
					category: 'Foo'
					instVarNames: #()
					classInstVarNames: #()
					classVarNames: #()
					poolDictionaryNames: #()
					comment: '') printString
		equals: 'a CypressClassDefinition(Foo)'