Class {
	#name : 'RwInvalidClassCategoryConventionErrorNotification',
	#superclass : 'RwNotification',
	#instVars : [
		'classDefinition',
		'packageConvention'
	],
	#category : 'Rowan-Core'
}

{ #category : 'instance creation' }
RwInvalidClassCategoryConventionErrorNotification class >> signalWithClassDefinition: aClassDefinition packageConvention: aString [

	^ self new
			classDefinition: aClassDefinition;
			packageConvention: aString;
			signal
]

{ #category : 'private' }
RwInvalidClassCategoryConventionErrorNotification >> _errorMessage [

	^ 'The class category ', 
		self classDefinition category printString, 
		' for the class ', 
		self classDefinition name printString, 
		' violates the package convention ', 
		self packageConvention printString
]

{ #category : 'accessing' }
RwInvalidClassCategoryConventionErrorNotification >> classDefinition [

	^ classDefinition
]

{ #category : 'accessing' }
RwInvalidClassCategoryConventionErrorNotification >> classDefinition: aClassDefinition [

	classDefinition := aClassDefinition
]

{ #category : 'Handling' }
RwInvalidClassCategoryConventionErrorNotification >> defaultAction [
 
	^ Error signal: self _errorMessage
]

{ #category : 'accessing' }
RwInvalidClassCategoryConventionErrorNotification >> packageConvention [

	^ packageConvention
]

{ #category : 'accessing' }
RwInvalidClassCategoryConventionErrorNotification >> packageConvention: aString [

	packageConvention := aString
]
