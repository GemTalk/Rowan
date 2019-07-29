Class {
	#name : 'RwNonExtensionProtocolExtensionMethodErrorNotification',
	#superclass : 'RwInvalidMethodProtocolConventionErrorNotification',
	#category : 'Rowan-Core'
}

{ #category : 'private' }
RwNonExtensionProtocolExtensionMethodErrorNotification >> _errorMessage [

	^ super _errorMessage, 'The protocol should start with a *, as the method IS an extension method.'
]

{ #category : 'Handling' }
RwNonExtensionProtocolExtensionMethodErrorNotification >> classCategoryNotificationDo: classCategoryBlock nonExtensionMethodNoficationDo: nonExtensionMethodBlock extensionMethodPackageMismatchNoficationDo: extensionMethodPackageMismatchBlock  extensionMethodNonExtensionProtocolNoficationDo: extensionMethodNonExtensionProtocolBlock [
	"extension method protocol doesn't start with a *"

	extensionMethodNonExtensionProtocolBlock value
]
