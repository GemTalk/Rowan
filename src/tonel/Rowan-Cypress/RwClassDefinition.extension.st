Extension { #name : 'RwClassDefinition' }

{ #category : '*rowan-cypress' }
RwClassDefinition >> category [

	^ properties at: 'category' ifAbsent: [  ]
]

{ #category : '*rowan-cypress' }
RwClassDefinition >> category: aString [

	^ properties at: 'category' put: aString
]

{ #category : '*rowan-cypress' }
RwClassDefinition >> classInstVarNames [
  ^ properties at: 'classinstvars'
]

{ #category : '*rowan-cypress' }
RwClassDefinition >> comment [
  ^ properties at: 'comment' ifAbsent: ['']
]

{ #category : '*rowan-cypress' }
RwClassDefinition class >> fromCypressClassStructure: aCypressClassStructure [
  ^ self new
    fromCypressClassStructure: aCypressClassStructure;
    yourself
]

{ #category : '*rowan-cypress' }
RwClassDefinition >> fromCypressClassStructure: aCypressClassStructure [
  properties
    at: 'category' put: aCypressClassStructure category;
    at: 'classinstvars' put: aCypressClassStructure classInstanceVariableNames;
    at: 'classvars' put: aCypressClassStructure classVariableNames;
    at: 'comment' put: aCypressClassStructure comment;
    at: 'instvars' put: aCypressClassStructure instanceVariableNames;
    at: 'name' put: aCypressClassStructure className;
    at: 'pools' put: aCypressClassStructure poolDictionaryNames;
    at: 'superclass' put: aCypressClassStructure superclassName;
    yourself.
  aCypressClassStructure subclassType isEmpty
    ifTrue: [ properties at: 'type' put: 'normal' ]
    ifFalse: [ properties at: 'type' put: aCypressClassStructure subclassType ].
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

{ #category : '*rowan-cypress' }
RwClassDefinition >> poolDictionaryNames [
  ^ properties at: 'pools'
]

{ #category : '*rowan-cypress' }
RwClassDefinition >> subclassType [
  | type |
  type := properties at: 'type'.
  type = 'normal'
    ifTrue: [ ^ '' ].
  ^ type
]
