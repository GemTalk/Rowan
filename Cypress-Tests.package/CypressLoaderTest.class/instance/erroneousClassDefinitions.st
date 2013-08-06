"
notice: This work is protected by copyright. All rights reserved.
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
			comment: 'This class tries to define an instance variable which already exists in the superclass.'.
	}
