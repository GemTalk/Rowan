Extension { #name : 'RwSpecification' }

{ #category : '*rowan-gemstone-specifications-extensions' }
RwSpecification >> register [

	Rowan image 
		loadedProjectNamed: self specName
		ifAbsent: [
			| newLoadedProject |
			newLoadedProject := RwGsLoadedSymbolDictProject newForLoadSpecification: self.
			Rowan image addLoadedProject: newLoadedProject ]

]
