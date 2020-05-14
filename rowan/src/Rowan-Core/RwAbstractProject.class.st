Class {
	#name : 'RwAbstractProject',
	#superclass : 'RowanInterface',
	#category : 'Rowan-Core'
}

{ #category : 'transitions' }
RwAbstractProject >> defined [
	self subclassResponsibility: #'defined'
]

{ #category : 'accessing' }
RwAbstractProject >> project [

	^ self
]

{ #category : 'transitions' }
RwAbstractProject >> resolve [
	self subclassResponsibility: #'resolve'
]
