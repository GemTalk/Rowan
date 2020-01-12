Class {
	#name : 'RwResolvedLoadSpecificationV2',
	#superclass : 'RwAbstractResolvedProjectV2',
	#category : 'Rowan-DefinitionsV2'
}

{ #category : 'accessing' }
RwResolvedLoadSpecificationV2 >> groupNames [
	"list of groups to be loaded. 
		Default is {'core' 'tests'}"

	^ self _loadSpecification groupNames
]

{ #category : 'accessing' }
RwResolvedLoadSpecificationV2 >> groupNames: anArray [
	"list of groups to be loaded. 
		Default is {'core' 'tests'}"

	self _loadSpecification groupNames: anArray
]
