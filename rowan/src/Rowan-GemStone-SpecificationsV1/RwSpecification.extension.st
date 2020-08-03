Extension { #name : 'RwSpecification' }

{ #category : '*rowan-gemstone-specificationsv1' }
RwSpecification >> register [

	Rowan image 
		loadedProjectNamed: self specName
		ifAbsent: [
			| newLoadedProject |
			newLoadedProject := RwGsLoadedSymbolDictProject newForLoadSpecification: self.
			Rowan image addLoadedProject: newLoadedProject ]

]
