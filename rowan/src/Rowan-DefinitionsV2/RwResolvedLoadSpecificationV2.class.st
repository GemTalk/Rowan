Class {
	#name : 'RwResolvedLoadSpecificationV2',
	#superclass : 'RwAbstractResolvedProjectV2',
	#category : 'Rowan-DefinitionsV2'
}

{ #category : 'accessing' }
RwResolvedLoadSpecificationV2 >> componentNames [
	"list of components to be loaded"

	^ self _loadSpecification componentNames
]

{ #category : 'accessing' }
RwResolvedLoadSpecificationV2 >> componentNames: anArray [
	"list of components to be loaded"

	self _loadSpecification componentNames: anArray
]

{ #category : 'accessing' }
RwResolvedLoadSpecificationV2 >> customConditionalAttributes: anArray [
	"set the custom conditional attributes"

	^ self _loadSpecification customConditionalAttributes: anArray
]

{ #category : 'printing' }
RwResolvedLoadSpecificationV2 >> printOn: aStream [
	super printOn: aStream.
	loadSpecification
		ifNotNil: [ 
			aStream
				nextPutAll: ' for ';
				nextPutAll: self _loadSpecification specName ]
]
