Extension { #name : 'RwComponentSpecification' }

{ #category : '*rowan-gemstone-components-extensions' }
RwComponentSpecification >> initialize [

	super initialize.
	defaultConfigurationNames := nil.
	defaultGroupNames := nil.
]

{ #category : '*rowan-gemstone-components-extensions' }
RwComponentSpecification >> register [

	Rowan image 
		loadedProjectNamed: self specName
		ifAbsent: [
			| newLoadedProject |
			newLoadedProject := RwGsLoadedSymbolDictComponentProject newForLoadSpecification: self.
			Rowan image addLoadedProject: newLoadedProject ]
]
