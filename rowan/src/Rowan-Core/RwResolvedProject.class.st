Class {
	#name : 'RwResolvedProject',
	#superclass : 'RwAbstractUnloadedProject',
	#category : 'Rowan-Core'
}

{ #category : 'instance creation' }
RwResolvedProject class >> fromDefinedProject: aDefinedProject [
	^ (self newNamed: aDefinedProject name)
		_resolvedProject: aDefinedProject _resolvedProject resolve;
		yourself
]

{ #category : 'accessing' }
RwResolvedProject >> _resolvedProject: aResolvedProject [
	resolvedProject := aResolvedProject
]

{ #category : 'accessing' }
RwResolvedProject >> repositoryRoot [
	^ self _resolvedProject repositoryRoot
]
