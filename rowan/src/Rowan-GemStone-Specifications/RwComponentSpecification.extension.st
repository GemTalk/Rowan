Extension { #name : 'RwComponentSpecification' }

{ #category : '*rowan-gemstone-specifications' }
RwComponentSpecification >> register [

	Rowan image 
		loadedProjectNamed: self specName
		ifAbsent: [
			| newLoadedProject |
			newLoadedProject := RwGsLoadedSymbolDictComponentProject newForLoadSpecification: self.
			Rowan image addLoadedProject: newLoadedProject ]
]
