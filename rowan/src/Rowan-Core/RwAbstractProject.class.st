Class {
	#name : 'RwAbstractProject',
	#superclass : 'RowanInterface',
	#category : 'Rowan-Core'
}

{ #category : 'accessing' }
RwAbstractProject >> project [

	^ self
]

{ #category : 'transitions' }
RwAbstractProject >> resolve [
	self subclassResponsibility: #'resolve'
]
