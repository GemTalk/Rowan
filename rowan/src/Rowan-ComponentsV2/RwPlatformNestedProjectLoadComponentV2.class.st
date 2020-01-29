Class {
	#name : 'RwPlatformNestedProjectLoadComponentV2',
	#superclass : 'RwSimpleNestedProjectLoadComponentV2',
	#category : 'Rowan-ComponentsV2'
}

{ #category : 'accessing' }
RwPlatformNestedProjectLoadComponentV2 >> addComponentNamed: aComponentName [
	self
		error:
			'a platform nested component may only reference package names'
]

{ #category : 'accessing' }
RwPlatformNestedProjectLoadComponentV2 >> addProjectNamed: aProjectName [
	self
		error:
			'a platform nested component may only reference package names'
]

{ #category : 'accessing' }
RwPlatformNestedProjectLoadComponentV2 >> componentNames [
	"a platform nested component may only reference package names --- OR conditionals may only be used by a leaf node"

	^ #()
]

{ #category : 'accessing' }
RwPlatformNestedProjectLoadComponentV2 >> condition: anArray [
	true
		ifTrue: [ 
			anArray _isArray
				ifFalse: [ self error: 'The condition is constrained to be an array' ] ].
	condition := anArray
]

{ #category : 'initialization' }
RwPlatformNestedProjectLoadComponentV2 >> initialize [
	super initialize.
	componentNames := projectNames := nil
]

{ #category : 'initialization' }
RwPlatformNestedProjectLoadComponentV2 >> initializeForExport [
	"if spec is to be exported, clear out any of the fields that represent state that should 
	not be shared"

	"for export, the keys in the dictionaries of the structures need to be put into canonical order"

	super initializeForExport.
	projectNames := componentNames := nil
]

{ #category : 'accessing' }
RwPlatformNestedProjectLoadComponentV2 >> projectNames [
	"a platform nested component may only reference package names --- OR conditionals may only be used by a leaf node"

	^ #()
]
