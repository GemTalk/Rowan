Class {
	#name : 'RowanGsMethod',
	#superclass : 'RowanGsObject',
	#instVars : [
		'behavior',
		'behaviorName',
		'breakPoints',
		'category',
		'isReadOnly',
		'originalSource',
		'source',
		'stepPoints',
		'unimplementedSelectors',
		'warnings'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'accessing' }
RowanGsMethod >> behavior [

	^behavior

]

{ #category : 'accessing' }
RowanGsMethod >> behavior: anObject [

	behavior := anObject

]

{ #category : 'accessing' }
RowanGsMethod >> behaviorName [

	^behaviorName

]

{ #category : 'accessing' }
RowanGsMethod >> behaviorName: anObject [

	behaviorName := anObject

]

{ #category : 'accessing' }
RowanGsMethod >> breakPoints [

	^breakPoints

]

{ #category : 'accessing' }
RowanGsMethod >> breakPoints: anObject [

	breakPoints := anObject

]

{ #category : 'accessing' }
RowanGsMethod >> category [

	^category

]

{ #category : 'accessing' }
RowanGsMethod >> category: anObject [

	category := anObject

]

{ #category : 'initialization' }
RowanGsMethod >> initialize [

	stepPoints := Dictionary new

]

{ #category : 'accessing' }
RowanGsMethod >> isReadOnly [

	^isReadOnly

]

{ #category : 'accessing' }
RowanGsMethod >> isReadOnly: anObject [

	isReadOnly := anObject

]

{ #category : 'accessing' }
RowanGsMethod >> originalSource [

	^originalSource

]

{ #category : 'accessing' }
RowanGsMethod >> originalSource: anObject [

	originalSource := anObject

]

{ #category : 'accessing' }
RowanGsMethod >> source [

	^source

]

{ #category : 'accessing' }
RowanGsMethod >> source: anObject [

	source := anObject

]

{ #category : 'accessing' }
RowanGsMethod >> stepPoints [

	^stepPoints

]

{ #category : 'accessing' }
RowanGsMethod >> stepPoints: anObject [

	stepPoints := anObject

]

{ #category : 'accessing' }
RowanGsMethod >> unimplementedSelectors [

	^unimplementedSelectors

]

{ #category : 'accessing' }
RowanGsMethod >> unimplementedSelectors: anObject [

	unimplementedSelectors := anObject

]

{ #category : 'accessing' }
RowanGsMethod >> warnings [

	^warnings

]

{ #category : 'accessing' }
RowanGsMethod >> warnings: anObject [

	warnings := anObject

]
