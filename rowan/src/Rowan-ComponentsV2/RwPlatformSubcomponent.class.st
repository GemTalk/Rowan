Class {
	#name : 'RwPlatformSubcomponent',
	#superclass : 'RwSubcomponent',
	#category : 'Rowan-ComponentsV2'
}

{ #category : 'accessing' }
RwPlatformSubcomponent >> addComponentNamed: aComponentName [
	self
		error:
			'a platform nested component may only reference package names'
]

{ #category : 'accessing' }
RwPlatformSubcomponent >> addProjectNamed: aProjectName [
	self
		error:
			'a platform nested component may only reference package names'
]

{ #category : 'accessing' }
RwPlatformSubcomponent >> componentNames [
	"a platform nested component may only reference package names --- OR conditionals may only be used by a leaf node"

	^ #()
]

{ #category : 'accessing' }
RwPlatformSubcomponent >> condition: anArray [
	anArray _isArray
		ifFalse: [ self error: 'The condition is constrained to be an array' ].
	condition := anArray
]

{ #category : 'accessing' }
RwPlatformSubcomponent >> conditionalPropertyMatchers [
	| conditionalPropertyMatchers |
	conditionalPropertyMatchers := Dictionary new
		at:
				(self condition
						collect: [ :aCondition | self _platformPatternMatcherFor: aCondition ])
			put: {};
		yourself.
	^ conditionalPropertyMatchers
]

{ #category : 'initialization' }
RwPlatformSubcomponent >> initialize [
	super initialize.
	componentNames := projectNames := nil
]

{ #category : 'initialization' }
RwPlatformSubcomponent >> initializeForExport [
	"if spec is to be exported, clear out any of the fields that represent state that should 
	not be shared"

	"for export, the keys in the dictionaries of the structures need to be put into canonical order"

	super initializeForExport.
	projectNames := componentNames := nil
]

{ #category : 'accessing' }
RwPlatformSubcomponent >> projectNames [
	"a platform nested component may only reference package names --- OR conditionals may only be used by a leaf node"

	^ #()
]
