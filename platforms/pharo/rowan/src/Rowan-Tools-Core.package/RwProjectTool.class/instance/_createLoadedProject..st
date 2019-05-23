private
_createLoadedProject: aRwProjectReference

	Rowan image 
		loadedProjectNamed: aRwProjectReference projectAlias
		ifAbsent: [
			| newLoadedProject |
			newLoadedProject := RwGsLoadedSymbolDictProject newForProjectReference: aRwProjectReference.
			Rowan image addLoadedProject: newLoadedProject ]