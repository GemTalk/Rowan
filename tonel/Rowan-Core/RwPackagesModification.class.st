"
A modification showing the difference between the classes or classExtensions of a package. The elements are classes. Key is class name.
"
Class {
	#name : 'RwPackagesModification',
	#superclass : 'RwElementsModification',
	#category : 'Rowan-Core'
}

{ #category : 'visiting' }
RwPackagesModification >> acceptVisitor: aVisitor [

	^ aVisitor visitPackagesModification: self
]
