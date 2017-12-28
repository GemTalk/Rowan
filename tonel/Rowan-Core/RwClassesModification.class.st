"
A modification showing the difference between the classes or classExtensions of a package. The elements are classes. Key is class name.
"
Class {
	#name : 'RwClassesModification',
	#superclass : 'RwElementsModification',
	#category : 'Rowan-Core'
}

{ #category : 'visiting' }
RwClassesModification >> acceptVisitor: aVisitor [

	^aVisitor visitClassesModification: self
]
