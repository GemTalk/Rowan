Class {
	#name : 'RwCypressPackageStructure',
	#superclass : 'CypressPackageStructure',
	#category : 'Rowan-Cypress'
}

{ #category : 'initialization' }
RwCypressPackageStructure >> fromPackage: aRwPackageDefinition [
  | classStructure |
  aRwPackageDefinition classDefinitions
    do: [ :classDefinition | 
      classStructure := (CypressClassStructure fromClassDefinition: classDefinition)
        packageStructure: self.
      classDefinition instanceMethodDefinitions
        do: [ :methodDefinition | 
          | methodStructure |
          methodStructure := RwCypressMethodStructure new
            fromRwInstanceMethodDefinition: methodDefinition;
            packageStructure: self;
            classStructure: classStructure.
          classStructure instanceMethods
            at: methodDefinition selector
            put: methodStructure ].
      classDefinition classMethodDefinitions
        do: [ :methodDefinition | 
          | methodStructure |
          methodStructure := RwCypressMethodStructure new
            fromRwClassMethodDefinition: methodDefinition;
            packageStructure: self;
            classStructure: classStructure.
          classStructure classMethods
            at: methodDefinition selector
            put: methodStructure ].
      self classes add: classStructure ].
  aRwPackageDefinition classExtensions
    do: [ :extensionDefinition | self halt: 'not yet implemented' ].
  name := aRwPackageDefinition key , self packageExtension.
  properties := Dictionary new
]

{ #category : 'initialization' }
RwCypressPackageStructure >> fromSnapshot: snapshot [
  | classMap classDefinitions |
  classDefinitions := OrderedCollection new.
  classMap := Dictionary new.
  snapshot
    do: [ :definition | 
      definition
        classDefinition: [ :classDefinition | classDefinitions add: classDefinition ]
        methodDefinition: [ :methodDefinition | 
          (classMap at: methodDefinition className ifAbsentPut: [ Set new ])
            add: methodDefinition ] ].
  self name: self name from: classDefinitions classMap: classMap
]

{ #category : 'snapshotting' }
RwCypressPackageStructure >> rwSnapshot [
  | definitions rwPackageDefinition |
  rwPackageDefinition := RwPackageDefinition new
    properties:
      (Dictionary new
        at: 'name' put: self packageName;
        yourself).
  self classes
    do: [ :classStructure | 
      | rwClassDef |
      rwClassDef := RwClassDefinition fromCypressClassStructure: classStructure.
      rwPackageDefinition addClassDefinition: rwClassDef ].
  self extensions
    do: [ :classStructure | 
      self halt: 'not yet implemented'.
      (classStructure instanceMethods
        asSortedCollection: [ :a :b | a selector <= b selector ])
        do: [ :methodStructure | definitions add: methodStructure asCypressMethodDefinition ].
      (classStructure classMethods
        asSortedCollection: [ :a :b | a selector <= b selector ])
        do: [ :methodStructure | definitions add: methodStructure asCypressMethodDefinition ] ].
  ^ rwPackageDefinition
]
