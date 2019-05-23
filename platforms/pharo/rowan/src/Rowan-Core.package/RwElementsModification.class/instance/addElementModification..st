modifications
addElementModification: aModification

	aModification isEmpty
		ifFalse: [elementsModified at: aModification key put: aModification]