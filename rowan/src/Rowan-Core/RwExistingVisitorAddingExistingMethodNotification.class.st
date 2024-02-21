Class {
	#name : 'RwExistingVisitorAddingExistingMethodNotification',
	#superclass : 'RwNotification',
	#instVars : [
		'incomingProject',
		'incomingPackage',
		'loadedMethod',
		'methodDefinition'
	],
	#category : 'Rowan-Core'
}

{ #category : 'handling' }
RwExistingVisitorAddingExistingMethodNotification >> defaultAction [

	self error: 
		'Visitor adding a method ', 
		methodDefinition name printString, 
		' that already exists in the project ', 
		self loadedProject name printString, 
		', but the project was not included in the load'
]

{ #category : 'accessing' }
RwExistingVisitorAddingExistingMethodNotification >> incomingPackage [
	^incomingPackage
]

{ #category : 'accessing' }
RwExistingVisitorAddingExistingMethodNotification >> incomingPackage: aPackageDefinition [
	incomingPackage := aPackageDefinition
]

{ #category : 'accessing' }
RwExistingVisitorAddingExistingMethodNotification >> incomingProject [
	^incomingProject
]

{ #category : 'accessing' }
RwExistingVisitorAddingExistingMethodNotification >> incomingProject: aProjectDefinition [
	incomingProject := aProjectDefinition
]

{ #category : 'accessing' }
RwExistingVisitorAddingExistingMethodNotification >> loadedMethod [
	^loadedMethod
]

{ #category : 'accessing' }
RwExistingVisitorAddingExistingMethodNotification >> loadedMethod: object [
	loadedMethod := object
]

{ #category : 'accessing' }
RwExistingVisitorAddingExistingMethodNotification >> loadedPackage [
	^ loadedMethod loadedPackage
]

{ #category : 'accessing' }
RwExistingVisitorAddingExistingMethodNotification >> loadedProject [
	^ loadedMethod loadedProject
]

{ #category : 'accessing' }
RwExistingVisitorAddingExistingMethodNotification >> methodDefinition [
	^methodDefinition
]

{ #category : 'accessing' }
RwExistingVisitorAddingExistingMethodNotification >> methodDefinition: object [
	methodDefinition := object
]

{ #category : 'accessing' }
RwExistingVisitorAddingExistingMethodNotification >> theClass [
	^ self loadedMethod handle inClass
]
