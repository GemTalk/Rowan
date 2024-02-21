Class {
	#name : 'RwExistingVisitorAddingExistingClassNotification',
	#superclass : 'RwNotification',
	#instVars : [
		'incomingProject',
		'incomingPackage',
		'theClass',
		'classDefinition',
		'loadedClass',
		'loadedProject'
	],
	#category : 'Rowan-Core'
}

{ #category : 'accessing' }
RwExistingVisitorAddingExistingClassNotification >> classDefinition [

	^ classDefinition

]

{ #category : 'accessing' }
RwExistingVisitorAddingExistingClassNotification >> classDefinition: aClassDefinition [

	classDefinition := aClassDefinition

]

{ #category : 'handling' }
RwExistingVisitorAddingExistingClassNotification >> defaultAction [

	loadedProject ifNil: [ ^ self error: 'Visitor adding a class ', classDefinition name printString, ' that already exists' ].
	self error: 
		'Visitor adding a class ', 
		classDefinition name printString, 
		' that already exists in the project ', 
		loadedProject name printString, 
		', but the project was not included in the load'

]

{ #category : 'accessing' }
RwExistingVisitorAddingExistingClassNotification >> incomingPackage [
	^incomingPackage
]

{ #category : 'accessing' }
RwExistingVisitorAddingExistingClassNotification >> incomingPackage: object [
	incomingPackage := object
]

{ #category : 'accessing' }
RwExistingVisitorAddingExistingClassNotification >> incomingProject [
	^incomingProject
]

{ #category : 'accessing' }
RwExistingVisitorAddingExistingClassNotification >> incomingProject: object [
	incomingProject := object
]

{ #category : 'accessing' }
RwExistingVisitorAddingExistingClassNotification >> loadedClass [
	^loadedClass
]

{ #category : 'accessing' }
RwExistingVisitorAddingExistingClassNotification >> loadedClass: object [
	loadedClass := object
]

{ #category : 'accessing' }
RwExistingVisitorAddingExistingClassNotification >> loadedPackage [
	^ loadedClass loadedPackage
]

{ #category : 'accessing' }
RwExistingVisitorAddingExistingClassNotification >> loadedProject [

	^ loadedProject

]

{ #category : 'accessing' }
RwExistingVisitorAddingExistingClassNotification >> loadedProject: aLoadedProject [

	loadedProject := aLoadedProject

]

{ #category : 'accessing' }
RwExistingVisitorAddingExistingClassNotification >> theClass [
	^theClass
]

{ #category : 'accessing' }
RwExistingVisitorAddingExistingClassNotification >> theClass: object [
	theClass := object
]
