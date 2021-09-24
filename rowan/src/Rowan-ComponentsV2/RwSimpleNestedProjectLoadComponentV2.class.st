Class {
	#name : 'RwSimpleNestedProjectLoadComponentV2',
	#superclass : 'RwSimpleProjectLoadComponentV2',
	#category : 'Rowan-ComponentsV2'
}

{ #category : 'ston' }
RwSimpleNestedProjectLoadComponentV2 >> _stonReplacementClass [
	^ RwSubcomponent
]

{ #category : 'visiting' }
RwSimpleNestedProjectLoadComponentV2 >> acceptNestedVisitor: aVisitor [
	^ aVisitor visitComponent: self
]

{ #category : 'visiting' }
RwSimpleNestedProjectLoadComponentV2 >> acceptVisitor: aVisitor [
	^ self
		error:
			'nested component cannot be used as a top-level component. The receiver is nested inside of top-level components'
]

{ #category : 'accessing' }
RwSimpleNestedProjectLoadComponentV2 >> label [
	^ self basename , ' (' , self condition , ')'
]
