Class {
	#name : 'RwExistingVisitorAddingExistingClassNotification',
	#superclass : 'RwNotification',
	#type : 'variable',
	#instVars : [
		'classDefinition',
		'loadedProject'
	],
	#gs_options : [
		'disallowGciStore'
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
RwExistingVisitorAddingExistingClassNotification >> loadedProject [

	^ loadedProject

]

{ #category : 'accessing' }
RwExistingVisitorAddingExistingClassNotification >> loadedProject: aLoadedProject [

	loadedProject := aLoadedProject

]
