Class {
	#name : 'RwCypressPackageStructure',
	#superclass : 'CypressPackageStructure',
	#category : 'Rowan-Cypress-Core'
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
		do: [ :extensionDefinition | 
			classStructure := (CypressClassStructure named: extensionDefinition name)
				isClassExtension: true;
				packageStructure: self.
			extensionDefinition instanceMethodDefinitions
				do: [ :methodDefinition | 
					| methodStructure |
					methodStructure := RwCypressMethodStructure new
						fromRwInstanceMethodDefinition: methodDefinition;
						packageStructure: self;
						classStructure: classStructure.
					classStructure instanceMethods
						at: methodDefinition selector
						put: methodStructure ].
			extensionDefinition classMethodDefinitions
				do: [ :methodDefinition | 
					| methodStructure |
					methodStructure := RwCypressMethodStructure new
						fromRwClassMethodDefinition: methodDefinition;
						packageStructure: self;
						classStructure: classStructure.
					classStructure classMethods
						at: methodDefinition selector
						put: methodStructure ].
			self extensions add: classStructure ].
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

	| rwPackageDefinition |
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
			| rwClassExtensionDef |
			rwClassExtensionDef := RwClassExtensionDefinition
				newForClassNamed: classStructure className.
			(classStructure instanceMethods
				asSortedCollection: [ :a :b | a selector <= b selector ])
				do: [ :methodStructure | 
					rwClassExtensionDef
						addInstanceMethodDefinition:
							(self _methodDefinitionClass fromCypressMethod: methodStructure) ].
			(classStructure classMethods
				asSortedCollection: [ :a :b | a selector <= b selector ])
				do: [ :methodStructure | 
					rwClassExtensionDef
						addClassMethodDefinition:
							(self _methodDefinitionClass fromCypressMethod: methodStructure) ].
			rwPackageDefinition addClassExtensionDefinition: rwClassExtensionDef ].
	^ rwPackageDefinition
]

{ #category : 'snapshotting' }
RwCypressPackageStructure >> snapshot [

	| definitions |
	definitions := OrderedCollection new.
	self classes
		do: [ :classStructure | 
			definitions add: classStructure asCypressClassDefinition.
			(classStructure instanceMethods
				asSortedCollection: [ :a :b | a selector <= b selector ])
				do: [ :methodStructure | definitions add: methodStructure asCypressMethodDefinition ].
			(classStructure classMethods
				asSortedCollection: [ :a :b | a selector <= b selector ])
				do: [ :methodStructure | definitions add: methodStructure asCypressMethodDefinition ] ].
	self extensions
		do: [ :classStructure | 
			(classStructure instanceMethods
				asSortedCollection: [ :a :b | a selector <= b selector ])
				do: [ :methodStructure | 
					| mdef |
					mdef := methodStructure asCypressMethodDefinition.
					mdef isExtensionMethod: true.
					definitions add: mdef ].
			(classStructure classMethods
				asSortedCollection: [ :a :b | a selector <= b selector ])
				do: [ :methodStructure | 
					| mdef |
					mdef := methodStructure asCypressMethodDefinition.
					mdef isExtensionMethod: true.
					definitions add: mdef ] ].
	^ CypressSnapshot definitions: definitions
]
