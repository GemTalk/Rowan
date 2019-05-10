Class {
	#name : 'RwExtensionProtocolExtensionMethodPackageMismatchErrorNotification',
	#superclass : 'RwInvalidMethodProtocolConventionErrorNotification',
	#category : 'Rowan-Core'
}

{ #category : 'private' }
RwExtensionProtocolExtensionMethodPackageMismatchErrorNotification >> _errorMessage [

	^ super _errorMessage, 'The extension protocol does not match the package of the extension method.'
]

{ #category : 'Handling' }
RwExtensionProtocolExtensionMethodPackageMismatchErrorNotification >> classCategoryNotificationDo: classCategoryBlock nonExtensionMethodNoficationDo: nonExtensionMethodBlock extensionMethodPackageMismatchNoficationDo: extensionMethodPackageMismatchBlock  extensionMethodNonExtensionProtocolNoficationDo: extensionMethodNonExtensionProtocolBlock [
	"extension method protocol does start with a *, but does not match the package name"

	extensionMethodPackageMismatchBlock value
]
