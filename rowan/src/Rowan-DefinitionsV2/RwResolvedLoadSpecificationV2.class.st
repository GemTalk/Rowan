Class {
	#name : 'RwResolvedLoadSpecificationV2',
	#superclass : 'RwAbstractResolvedProjectV2',
	#category : 'Rowan-DefinitionsV2'
}

{ #category : 'accessing' }
RwResolvedLoadSpecificationV2 >> comment [
	^ self _loadSpecification comment
]

{ #category : 'accessing' }
RwResolvedLoadSpecificationV2 >> comment: aString [
	self _loadSpecification comment: aString
]

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
RwResolvedLoadSpecificationV2 >> customConditionalAttributes [
	"get the custom conditional attributes"

	^ self _loadSpecification customConditionalAttributes
]

{ #category : 'accessing' }
RwResolvedLoadSpecificationV2 >> customConditionalAttributes: anArray [
	"set the custom conditional attributes"

	^ self _loadSpecification customConditionalAttributes: anArray
]

{ #category : 'actions' }
RwResolvedLoadSpecificationV2 >> export [
	self specsRoot ensureCreateDirectory.
	self exportLoadSpecification
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

{ #category : 'accessing' }
RwResolvedLoadSpecificationV2 >> revision [
	^ self _loadSpecification revision
]

{ #category : 'accessing' }
RwResolvedLoadSpecificationV2 >> revision: aString [
	self _loadSpecification revision: aString
]

{ #category : 'accessing' }
RwResolvedLoadSpecificationV2 >> specName [
	^ self _loadSpecification specName
]

{ #category : 'accessing' }
RwResolvedLoadSpecificationV2 >> specName: aString [
	self _loadSpecification specName: aString
]
