Extension { #name : 'RwProjectReferenceDefinition' }

{ #category : '*rowan-gemstone-components-extensions' }
RwProjectReferenceDefinition >> register [

	Rowan image 
		loadedProjectNamed: self projectAlias
		ifAbsent: [
			| newLoadedProject |
			newLoadedProject := RwGsLoadedSymbolDictComponentProject newForProjectReferenceDefinition: self.
			Rowan image addLoadedProject: newLoadedProject ]
]
