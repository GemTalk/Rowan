Extension { #name : 'RwSpecification' }

{ #category : '*rowan-gemstone-specifications' }
RwSpecification >> register [

	Rowan image 
		loadedProjectNamed: self specName
		ifAbsent: [
			| newLoadedProject |
			newLoadedProject := RwGsLoadedSymbolDictProject newForLoadSpecification: self.
			Rowan image addLoadedProject: newLoadedProject ]

]
