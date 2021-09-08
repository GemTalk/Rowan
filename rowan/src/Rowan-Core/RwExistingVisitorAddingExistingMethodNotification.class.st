Class {
	#name : 'RwExistingVisitorAddingExistingMethodNotification',
	#superclass : 'RwNotification',
	#instVars : [
		'loadedMethod',
		'methodDefinition'
	],
	#category : 'Rowan-Core'
}

{ #category : 'accessing' }
RwExistingVisitorAddingExistingMethodNotification >> class [
	^ self loadedMethod handle inClass
]

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
RwExistingVisitorAddingExistingMethodNotification >> loadedMethod [
	^loadedMethod
]

{ #category : 'accessing' }
RwExistingVisitorAddingExistingMethodNotification >> loadedMethod: object [
	loadedMethod := object
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
