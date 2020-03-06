Extension { #name : 'RwLoadedProject' }

{ #category : '*rowan-corev1' }
RwLoadedProject >> asDefinition [

	"Create a Definition corresponding to what I define in the image."

	^ RwProjectDefinition
		withProperties: self propertiesForDefinition
		packageDefinitions: self loadedPackageDefinitions
]
