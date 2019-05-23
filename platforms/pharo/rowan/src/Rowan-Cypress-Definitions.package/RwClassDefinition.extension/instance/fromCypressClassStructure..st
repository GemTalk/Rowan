*rowan-cypress-definitions
fromCypressClassStructure: aCypressClassStructure

	properties
		at: 'category' put: aCypressClassStructure category;
		at: 'classinstvars' put: aCypressClassStructure classInstanceVariableNames;
		at: 'classvars' put: aCypressClassStructure classVariableNames;
		yourself.
	aCypressClassStructure comment ifNotNil: [ :cmt | properties at: 'comment' put: cmt ].
	aCypressClassStructure gs_options isEmpty ifFalse: [ properties at: 'gs_options' put: aCypressClassStructure gs_options ].
	aCypressClassStructure gs_constraints isEmpty ifFalse: [ properties at: 'gs_constraints' put: aCypressClassStructure gs_constraints ].
	properties
		at: 'instvars' put: aCypressClassStructure instanceVariableNames;
		at: 'name' put: aCypressClassStructure className;
		at: 'pools' put: aCypressClassStructure poolDictionaryNames;
		at: 'superclass' put: aCypressClassStructure superclassName;
		yourself.
	aCypressClassStructure subclassType isEmpty
		ifTrue: [ properties at: 'type' put: 'normal' ]
		ifFalse: [ properties at: 'type' put: aCypressClassStructure subclassType asString ].
	aCypressClassStructure instanceMethods
		do: [ :cypMethodDef | 
			self
				addInstanceMethodDefinition:
					(RwMethodDefinition fromCypressMethod: cypMethodDef) ].
	aCypressClassStructure classMethods
		do: [ :cypMethodDef | 
			self
				addClassMethodDefinition:
					(RwMethodDefinition fromCypressMethod: cypMethodDef) ]
