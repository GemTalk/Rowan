Extension { #name : 'RwResolvedProjectV2' }

{ #category : '*rowan-gemstone-definitions-extensionsv2' }
RwResolvedProjectV2 >> asLoadedSymbolDictProject [
	^ RwGsLoadedSymbolDictResolvedProjectV2 newForResolvedProject: self
]
