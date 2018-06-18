Extension { #name : 'RwClassDefinition' }

{ #category : '*rowan-cypress-core' }
RwClassDefinition >> category [

	^ properties at: 'category' ifAbsent: [  ]
]

{ #category : '*rowan-cypress-core' }
RwClassDefinition >> category: aString [

	^ properties at: 'category' put: aString
]

{ #category : '*rowan-cypress-core' }
RwClassDefinition >> classInstVarNames [
  ^ properties at: 'classinstvars'
]

{ #category : '*rowan-cypress-core' }
RwClassDefinition >> classInstVarNames: anArray [

	^ properties at: 'classinstvars' put: anArray
]

{ #category : '*rowan-cypress-core' }
RwClassDefinition >> comment [
  ^ properties at: 'comment' ifAbsent: ['']
]

{ #category : '*rowan-cypress-core' }
RwClassDefinition >> fromCypressClassStructure: aCypressClassStructure [

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

]

{ #category : '*rowan-cypress-core' }
RwClassDefinition >> poolDictionaryNames [
  ^ properties at: 'pools'
]

{ #category : '*rowan-cypress-core' }
RwClassDefinition >> subclassType [
  | type |
  type := properties at: 'type'.
  type = 'normal'
    ifTrue: [ ^ '' ].
  ^ type
]

{ #category : '*rowan-cypress-core' }
RwClassDefinition class >> fromCypressClassStructure: aCypressClassStructure [
  ^ self new
    fromCypressClassStructure: aCypressClassStructure;
    yourself
]
