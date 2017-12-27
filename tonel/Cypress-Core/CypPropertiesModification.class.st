"
A modification showing the difference between the properties of a single entity. The elements are associations of propertyName -> propertyValue. Key is property name.
"
Class {
	#name : 'CypPropertiesModification',
	#superclass : 'CypElementsModification',
	#category : 'Cypress-Core'
}

{ #category : 'visiting' }
CypPropertiesModification >> acceptVisitor: aVisitor [

	^aVisitor visitPropertiesModification: self
]
