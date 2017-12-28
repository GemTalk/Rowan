"
A modification showing the difference between the properties of a single entity. The elements are associations of propertyName -> propertyValue. Key is property name.
"
Class {
	#name : 'RwPropertiesModification',
	#superclass : 'RwElementsModification',
	#category : 'Rowan-Core'
}

{ #category : 'visiting' }
RwPropertiesModification >> acceptVisitor: aVisitor [

	^aVisitor visitPropertiesModification: self
]
