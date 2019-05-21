Class {
	#name : 'RwExtensionProtocolNonExtensionMethodErrorNotification',
	#superclass : 'RwInvalidMethodProtocolConventionErrorNotification',
	#category : 'Rowan-Core'
}

{ #category : 'private' }
RwExtensionProtocolNonExtensionMethodErrorNotification >> _errorMessage [

	^ super _errorMessage, 'The protocol should not start with a *, as the method is NOT an extension method.'
]

{ #category : 'Handling' }
RwExtensionProtocolNonExtensionMethodErrorNotification >> classCategoryNotificationDo: classCategoryBlock nonExtensionMethodNoficationDo: nonExtensionMethodBlock extensionMethodPackageMismatchNoficationDo: extensionMethodPackageMismatchBlock  extensionMethodNonExtensionProtocolNoficationDo: extensionMethodNonExtensionProtocolBlock [
	"method protocol starts with a * for non-extension method"

	nonExtensionMethodBlock value
]
