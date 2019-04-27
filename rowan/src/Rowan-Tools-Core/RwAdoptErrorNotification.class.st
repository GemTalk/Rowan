Class {
	#name : 'RwAdoptErrorNotification',
	#superclass : 'Notification',
	#instVars : [
		'className',
		'isClassExtension',
		'packageName'
	],
	#category : 'Rowan-Tools-Core'
}

{ #category : 'private' }
RwAdoptErrorNotification >> _errorMessage [

	self subclassResponsibility: #_errorMessage
]

{ #category : 'accessing' }
RwAdoptErrorNotification >> className [

	^ className
]

{ #category : 'accessing' }
RwAdoptErrorNotification >> className: aString [

	className := aString
]

{ #category : 'Handling' }
RwAdoptErrorNotification >> defaultAction [
 
	^ Error signal: self _errorMessage
]

{ #category : 'accessing' }
RwAdoptErrorNotification >> isClassExtension [

	^ isClassExtension
]

{ #category : 'accessing' }
RwAdoptErrorNotification >> isClassExtension: aBoolean [

	isClassExtension := aBoolean
]

{ #category : 'accessing' }
RwAdoptErrorNotification >> packageName [

	^ packageName
]

{ #category : 'accessing' }
RwAdoptErrorNotification >> packageName: aString [

	packageName := aString
]
