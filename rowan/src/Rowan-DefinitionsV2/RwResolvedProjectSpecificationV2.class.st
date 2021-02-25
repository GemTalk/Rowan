Class {
	#name : 'RwResolvedProjectSpecificationV2',
	#superclass : 'RwAbstractResolvedProjectV2',
	#category : 'Rowan-DefinitionsV2'
}

{ #category : 'accessubg' }
RwResolvedProjectSpecificationV2 >> componentsPath [
	^self _projectSpecification componentsPath
]

{ #category : 'accessubg' }
RwResolvedProjectSpecificationV2 >> componentsPath: aString [
	self _projectSpecification componentsPath: aString
]

{ #category : 'actions' }
RwResolvedProjectSpecificationV2 >> export [
	self exportProjectSpecification
]

{ #category : 'accessubg' }
RwResolvedProjectSpecificationV2 >> packagesPath [
	^self _projectSpecification packagesPath
]

{ #category : 'accessubg' }
RwResolvedProjectSpecificationV2 >> packagesPath: aString [
	self _projectSpecification packagesPath: aString
]

{ #category : 'printing' }
RwResolvedProjectSpecificationV2 >> printOn: aStream [
	super printOn: aStream.
	projectSpecification
		ifNotNil: [ 
			aStream
				nextPutAll: ' for ';
				nextPutAll: self _projectSpecification specName ]
]

{ #category : 'accessubg' }
RwResolvedProjectSpecificationV2 >> projectsPath [
	^self _projectSpecification projectsPath
]

{ #category : 'accessubg' }
RwResolvedProjectSpecificationV2 >> projectsPath: aString [
	self _projectSpecification projectsPath: aString
]

{ #category : 'accessubg' }
RwResolvedProjectSpecificationV2 >> specsPath [
	^self _projectSpecification specsPath
]

{ #category : 'accessubg' }
RwResolvedProjectSpecificationV2 >> specsPath: aString [
	self _projectSpecification specsPath: aString
]
