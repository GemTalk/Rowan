Class {
	#name : 'RwGsMethodExtensionSessionMethodSourceSymbolDictPatchV2',
	#superclass : 'RwGsMethodSourceSymbolDictPatchV2',
	#category : 'Rowan-GemStone-LoaderV2'
}

{ #category : 'installing' }
RwGsMethodExtensionSessionMethodSourceSymbolDictPatchV2 >> installSourcePatch [

	" update methodDictionary and update loadedMethod with new compiled method"

	self symbolDictionaryRegistry addRecompiledSessionMethodMethod: compiledMethod implementationClass: RwGsSymbolDictionaryRegistry_ImplementationV2.
	selector := compiledMethod selector
]
