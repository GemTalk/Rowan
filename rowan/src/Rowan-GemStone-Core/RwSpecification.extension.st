Extension { #name : 'RwSpecification' }

{ #category : '*rowan-gemstone-core' }
RwSpecification >> register [

	Rowan image 
		loadedProjectNamed: self specName
		ifAbsent: [
			| newLoadedProject |
			newLoadedProject := RwGsLoadedSymbolDictProject newForLoadSpecification: self.
			Rowan image addLoadedProject: newLoadedProject ]

]
