"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: utility
"
erroneousClassDefinitions

	^{
		CypressClassDefinition
			name: 'CypressMockBasicSubclassWithDuplicateInstanceVariable'
			superclassName: 'CypressMockBasic'
			category: 'Cypress-Mocks'
			instVarNames: #('name')
			classInstVarNames: #()
			classVarNames: #()
			poolDictionaryNames: #()
			comment: 'This class tries to define an instance variable which already exists in the superclass.'
			subclassType: ''.
	}
