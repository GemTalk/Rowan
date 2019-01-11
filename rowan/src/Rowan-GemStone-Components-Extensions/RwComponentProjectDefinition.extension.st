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
