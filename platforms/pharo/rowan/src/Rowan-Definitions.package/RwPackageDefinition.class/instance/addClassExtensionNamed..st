accessing
addClassExtensionNamed: className

	^ self addClassExtensionDefinition: (RwClassExtensionDefinition newForClassNamed: className)