Extension { #name : 'RwComponentProjectDefinition' }

{ #category : '*rowan-gemstone-components-extensions' }
RwComponentProjectDefinition >> register [

	Rowan image 
		loadedProjectNamed: self name
		ifAbsent: [
			| newLoadedProject |
			newLoadedProject := RwGsLoadedSymbolDictComponentProject newForProjectReferenceDefinition: self projectRef.
			Rowan image addLoadedProject: newLoadedProject ]
]

{ #category : '*rowan-gemstone-components-extensions' }
RwComponentProjectDefinition >> setSymbolDictName: symbolDictName forPackageNamed: packageName [

	^self projectRef setSymbolDictName: symbolDictName forPackageNamed: packageName
]
