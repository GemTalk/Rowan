Class {
	#name : 'RwInvalidClassCategoryConventionErrorNotification',
	#superclass : 'RwInvalidCategoryProtocolConventionErrorNotification',
	#instVars : [
		'classDefinition'
	],
	#category : 'Rowan-Core'
}

{ #category : 'instance creation' }
RwInvalidClassCategoryConventionErrorNotification class >> signalWithClassDefinition: aClassDefinition packageName: packageName packageConvention: aString [

	^ self new
			classDefinition: aClassDefinition;
			packageName:  packageName;
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

{ #category : 'Handling' }
RwInvalidClassCategoryConventionErrorNotification >> classCategoryNotificationDo: classCategoryBlock nonExtensionMethodNoficationDo: nonExtensionMethodBlock extensionMethodPackageMismatchNoficationDo: extensionMethodPackageMismatchBlock  extensionMethodNonExtensionProtocolNoficationDo: extensionMethodNonExtensionProtocolBlock [
	"class category does not match the package name"

	classCategoryBlock value
]

{ #category : 'accessing' }
RwInvalidClassCategoryConventionErrorNotification >> classDefinition [

	^ classDefinition
]

{ #category : 'accessing' }
RwInvalidClassCategoryConventionErrorNotification >> classDefinition: aClassDefinition [

	classDefinition := aClassDefinition
]
