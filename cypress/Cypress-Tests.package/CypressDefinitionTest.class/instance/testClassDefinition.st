"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
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
					comment: ''
					subclassType: '') printString
		equals: 'a CypressClassDefinition(Foo)'