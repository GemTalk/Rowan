Class {
	#name : 'RwEntitySetModification',
	#superclass : 'RwElementsModification',
	#category : 'Rowan-Core'
}

{ #category : 'enumerating' }
RwEntitySetModification >> classesModifiedDo: aBlock [

	"enumerate the classesModification dictionary of the receiver (package set modification or project set modification"

	self subclassResponsibility: #'classesModifiedDo:'
]
