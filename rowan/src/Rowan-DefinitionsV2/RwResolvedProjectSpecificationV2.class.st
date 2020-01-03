Class {
	#name : 'RwResolvedProjectSpecificationV2',
	#superclass : 'RwAbstractResolvedObjectV2',
	#category : 'Rowan-DefinitionsV2'
}

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
