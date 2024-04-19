Extension { #name : 'RwClassDefinition' }

{ #category : '*rowan-cypress-definitions' }
RwClassDefinition class >> fromCypressClassStructure: aCypressClassStructure [
  | res |
  (res := self new)
    fromCypressClassStructure: aCypressClassStructure .
  ^ res
]

{ #category : '*rowan-cypress-definitions' }
RwClassDefinition >> category [

	^ properties at: #'category' ifAbsent: [  ]
]

{ #category : '*rowan-cypress-definitions' }
RwClassDefinition >> category: aString [

	^ properties at: #'category' put: aString
]

{ #category : '*rowan-cypress-definitions' }
RwClassDefinition >> classInstVarNames [
  ^ properties at: #'classinstvars'
]

{ #category : '*rowan-cypress-definitions' }
RwClassDefinition >> classInstVarNames: anArray [

	^ properties at: #'classinstvars' put: anArray
]

{ #category : '*rowan-cypress-definitions' }
RwClassDefinition >> comment [
  ^ properties at: #'comment' ifAbsent: ['']
]

{ #category : '*rowan-cypress-definitions' }
RwClassDefinition >> fromCypressClassStructure: aCypressClassStructure [

	properties
		at: #'category' put: aCypressClassStructure category;
		at: #'classinstvars' put: aCypressClassStructure classInstanceVariableNames;
		at: #'classvars' put: aCypressClassStructure classVariableNames .

	aCypressClassStructure comment ifNotNil: [ :cmt | properties at: #'comment' put: cmt ].
	aCypressClassStructure gs_options isEmpty ifFalse: [ properties at: #'gs_options' put: aCypressClassStructure gs_options ].
	aCypressClassStructure gs_constraints isEmpty ifFalse: [ properties at: #'gs_constraints' put: aCypressClassStructure gs_constraints ].
	properties
		at: #'instvars' put: aCypressClassStructure instanceVariableNames;
		at: #'name' put: aCypressClassStructure className;
		at: #'pools' put: aCypressClassStructure poolDictionaryNames;
		at: #'superclass' put: aCypressClassStructure superclassName .

	aCypressClassStructure subclassType isEmpty
		ifTrue: [ properties at: #'type' put: 'normal' ]
		ifFalse: [ properties at: #'type' put: aCypressClassStructure subclassType asString ].
	aCypressClassStructure instanceMethods
		do: [ :cypMethodDef | 
			self
				addInstanceMethodDefinition:
					(self _methodDefinitionClass fromCypressMethod: cypMethodDef) ].
	aCypressClassStructure classMethods
		do: [ :cypMethodDef | 
			self
				addClassMethodDefinition:
					(self _methodDefinitionClass fromCypressMethod: cypMethodDef) ]
]

{ #category : '*rowan-cypress-definitions' }
RwClassDefinition >> poolDictionaryNames [
  ^ properties at: #'pools'
]

{ #category : '*rowan-cypress-definitions' }
RwClassDefinition >> subclassType [
  | type |
  type := properties at: #'type'.
  type = 'normal'
    ifTrue: [ ^ '' ].
  ^ type
]
