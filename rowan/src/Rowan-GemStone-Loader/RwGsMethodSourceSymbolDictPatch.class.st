Class {
	#name : 'RwGsMethodSourceSymbolDictPatch',
	#superclass : 'RwGsMethodPatch',
	#category : 'Rowan-GemStone-Loader'
}

{ #category : 'installing' }
RwGsMethodSourceSymbolDictPatch >> installSourcePatch [

	" update methodDictionary and update loadedMethod with new compiled method"

	self symbolDictionaryRegistry addRecompiledMethod: compiledMethod implementationClass: RwGsSymbolDictionaryRegistry_Implementation.
	selector := compiledMethod selector

]
