Class {
	#name : 'RwGsMethodExtensionSessionMethodSourceSymbolDictPatch',
	#superclass : 'RwGsMethodSourceSymbolDictPatch',
	#category : 'Rowan-GemStone-Core'
}

{ #category : 'installing' }
RwGsMethodExtensionSessionMethodSourceSymbolDictPatch >> installSourcePatch [

	" update methodDictionary and update loadedMethod with new compiled method"

	self symbolDictionaryRegistry addRecompiledSessionMethodMethod: compiledMethod.
	selector := compiledMethod selector
]
