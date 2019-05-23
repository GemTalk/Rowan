modifications
addElementModification: aModification

	aModification isEmpty
		ifFalse: [ 
			| anExtensionModification |
			anExtensionModification := aModification asExtensionMethodModification.
			anExtensionModification extendedClassName: self extendedClassName.
			elementsModified at: anExtensionModification key put: anExtensionModification ]
