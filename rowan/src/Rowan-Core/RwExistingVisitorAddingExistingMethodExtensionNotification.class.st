Class {
	#name : 'RwExistingVisitorAddingExistingMethodExtensionNotification',
	#superclass : 'RwNotification',
	#instVars : [
		'loadedMethod',
		'methodDefinition'
	],
	#category : 'Rowan-Core'
}

{ #category : 'accessing' }
RwExistingVisitorAddingExistingMethodExtensionNotification >> class [
	^ self loadedMethod handle inClass
]

{ #category : 'handling' }
RwExistingVisitorAddingExistingMethodExtensionNotification >> defaultAction [

	self error: 
		'Visitor adding a method ', 
		methodDefinition name printString, 
		' that already exists in the project ', 
		self loadedProject name printString, 
		', but the project was not included in the load'
]

{ #category : 'accessing' }
RwExistingVisitorAddingExistingMethodExtensionNotification >> loadedMethod [
	^loadedMethod
]

{ #category : 'accessing' }
RwExistingVisitorAddingExistingMethodExtensionNotification >> loadedMethod: object [
	loadedMethod := object
]

{ #category : 'accessing' }
RwExistingVisitorAddingExistingMethodExtensionNotification >> loadedProject [
	^ loadedMethod loadedProject
]

{ #category : 'accessing' }
RwExistingVisitorAddingExistingMethodExtensionNotification >> methodDefinition [
	^methodDefinition
]

{ #category : 'accessing' }
RwExistingVisitorAddingExistingMethodExtensionNotification >> methodDefinition: object [
	methodDefinition := object
]
