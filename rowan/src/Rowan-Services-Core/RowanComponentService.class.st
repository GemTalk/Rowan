Class {
	#name : 'RowanComponentService',
	#superclass : 'RowanService',
	#instVars : [
		'name',
		'componentServices',
		'packageServices',
		'projectService'
	],
	#category : 'Rowan-Services-Core'
}

{ #category : 'instance creation' }
RowanComponentService class >> forComponentNamed: componentName [

	| inst |
	inst := self new name: componentName.
	inst initialize. 
	^inst
]

{ #category : 'initialization' }
RowanComponentService >> initialize [
	" to do "
	super initialize
]

{ #category : 'accessing' }
RowanComponentService >> name [
	^name
]

{ #category : 'accessing' }
RowanComponentService >> name: object [
	name := object
]
