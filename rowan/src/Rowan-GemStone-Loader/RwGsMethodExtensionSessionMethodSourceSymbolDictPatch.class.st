Class {
	#name : 'RwGsMethodExtensionSessionMethodSourceSymbolDictPatch',
	#superclass : 'RwGsMethodSourceSymbolDictPatch',
	#category : 'Rowan-GemStone-Loader'
}

{ #category : 'installing' }
RwGsMethodExtensionSessionMethodSourceSymbolDictPatch >> installSourcePatch [

	" update methodDictionary and update loadedMethod with new compiled method"

	self symbolDictionaryRegistry addRecompiledSessionMethodMethod: compiledMethod implementationClass: RwGsSymbolDictionaryRegistry_Implementation.
	selector := compiledMethod selector

]
