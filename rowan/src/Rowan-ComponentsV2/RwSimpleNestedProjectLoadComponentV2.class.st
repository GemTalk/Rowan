Class {
	#name : 'RwSimpleNestedProjectLoadComponentV2',
	#superclass : 'RwSimpleProjectLoadComponentV2',
	#category : 'Rowan-ComponentsV2'
}

{ #category : 'visiting' }
RwSimpleNestedProjectLoadComponentV2 >> acceptNestedVisitor: aVisitor [
	^ aVisitor visitSimpleProjectLoadComponent: self
]

{ #category : 'visiting' }
RwSimpleNestedProjectLoadComponentV2 >> acceptVisitor: aVisitor [
	^ self
		error:
			'nested component cannot be used as a top-level configuration. The receiver is nested inside of top-level components'
]
