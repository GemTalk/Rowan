Class {
	#name : 'RwAbstractProject',
	#superclass : 'RowanInterface',
	#category : 'Rowan-Core'
}

{ #category : 'private' }
RwAbstractProject >> _concreteProject [
	self subclassResponsibility: #'_concreteProject'
]

{ #category : 'private' }
RwAbstractProject >> _loadSpecification [
	^ self _concreteProject _loadSpecification
]

{ #category : 'transitions' }
RwAbstractProject >> defined [
	self subclassResponsibility: #'defined'
]

{ #category : 'accessing' }
RwAbstractProject >> project [

	^ self
]
