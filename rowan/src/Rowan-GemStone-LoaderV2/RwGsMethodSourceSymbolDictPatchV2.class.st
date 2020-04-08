Class {
	#name : 'RwGsMethodSourceSymbolDictPatchV2',
	#superclass : 'RwGsMethodPatchV2',
	#category : 'Rowan-GemStone-LoaderV2'
}

{ #category : 'installing' }
RwGsMethodSourceSymbolDictPatchV2 >> installSourcePatch [

	" update methodDictionary and update loadedMethod with new compiled method"

	self symbolDictionaryRegistry addRecompiledMethod: compiledMethod implementationClass: RwGsSymbolDictionaryRegistry_ImplementationV2.
	selector := compiledMethod selector
]
