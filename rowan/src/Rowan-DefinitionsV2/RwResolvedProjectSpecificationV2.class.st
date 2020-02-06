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

{ #category : 'accessubg' }
RwResolvedProjectSpecificationV2 >> packageConvention [
	"
		RowanHybrid	- [default] Class category is package name, method protocol with leading $* is case insensitive package name
		Monticello		- Class category is package name, method protocol with leading $* begins with case insensitive package name
		Rowan			- Class category and method protocol are not overloaded with packaging information
	"

	^ self _projectSpecification packageConvention
]

{ #category : 'accessubg' }
RwResolvedProjectSpecificationV2 >> packageConvention: aString [
	"
		RowanHybrid	- [default] Class category is package name, method protocol with leading $* is case insensitive package name
		Monticello		- Class category is package name, method protocol with leading $* begins with case insensitive package name
		Rowan			- Class category and method protocol are not overloaded with packaging information
	"

	self _projectSpecification packageConvention: aString
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
	self _projectSpecification ifNil: [ ^ super printOn: aStream ].

	self _projectSpecification printOn: aStream
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
