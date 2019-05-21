Class {
	#name : 'RwInvalidCategoryProtocolConventionErrorNotification',
	#superclass : 'RwNotification',
	#instVars : [
		'packageName',
		'packageConvention'
	],
	#category : 'Rowan-Core'
}

{ #category : 'private' }
RwInvalidCategoryProtocolConventionErrorNotification >> _errorMessage [

	^ self subclassResponsibility: #_errorMessage
]

{ #category : 'Handling' }
RwInvalidCategoryProtocolConventionErrorNotification >> classCategoryNotificationDo: classCategoryBlock nonExtensionMethodNoficationDo: nonExtensionMethodBlock extensionMethodPackageMismatchNoficationDo: extensionMethodPackageMismatchBlock  extensionMethodNonExtensionProtocolNoficationDo: extensionMethodNonExtensionProtocolBlock [
	"helper method for use in exception handling block ... avoid isKindOf:"

	
	self subclassResponsibility: #classCategoryNotificationDo:nonExtensionMethodNoficationDo:extensionMethodPackageMismatchNoficationDo:extensionMethodNonExtensionProtocolNoficationDo:
]

{ #category : 'Handling' }
RwInvalidCategoryProtocolConventionErrorNotification >> defaultAction [
 
	^ Error signal: self _errorMessage
]

{ #category : 'accessing' }
RwInvalidCategoryProtocolConventionErrorNotification >> packageConvention [

	^ packageConvention
]

{ #category : 'accessing' }
RwInvalidCategoryProtocolConventionErrorNotification >> packageConvention: aString [

	packageConvention := aString
]

{ #category : 'accessing' }
RwInvalidCategoryProtocolConventionErrorNotification >> packageName [

	^ packageName
]

{ #category : 'accessing' }
RwInvalidCategoryProtocolConventionErrorNotification >> packageName: aString [

	packageName := aString
]
