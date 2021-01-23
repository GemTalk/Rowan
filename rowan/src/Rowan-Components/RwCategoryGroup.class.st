Class {
	#name : 'RwCategoryGroup',
	#superclass : 'RwAbstractComponent',
	#category : 'Rowan-Components'
}

{ #category : 'visiting' }
RwCategoryGroup >> acceptNestedVisitor: aVisitor [
	^ aVisitor visitCategoryGroupComponent: self
]

{ #category : 'visiting' }
RwCategoryGroup >> acceptVisitor: aVisitor [
	^ self
		error:
			'Category group cannot be independently loaded. The receiver is nested inside of other components'
]
