Class {
	#name : 'RwPlatformNestedProjectLoadComponentV2',
	#superclass : 'RwSimpleNestedProjectLoadComponentV2',
	#category : 'Rowan-ComponentsV2'
}

{ #category : 'ston' }
RwPlatformNestedProjectLoadComponentV2 >> _stonReplacementClass [
	^ RwPlatformSubcomponent
]

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
	anArray _isArray
		ifFalse: [ self error: 'The condition is constrained to be an array' ].
	condition := anArray
]

{ #category : 'accessing' }
RwPlatformNestedProjectLoadComponentV2 >> conditionalPropertyMatchers [
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
RwPlatformNestedProjectLoadComponentV2 >> label [
	| strm |
	strm := WriteStream on: String new.
	strm
		nextPutAll: self basename;
		space;
		nextPut: ${.
	self condition
		do: [ :cond | 
			strm
				space;
				nextPutAll: cond asString ].
	strm
		space;
		nextPut: $}.
	^ strm contents
]

{ #category : 'accessing' }
RwPlatformNestedProjectLoadComponentV2 >> projectNames [
	"a platform nested component may only reference package names --- OR conditionals may only be used by a leaf node"

	^ #()
]
