Class {
	#name : 'RwGsMethodSourceSymbolDictPatch',
	#superclass : 'RwGsMethodPatch',
	#category : 'Rowan-GemStone'
}

{ #category : 'installing' }
RwGsMethodSourceSymbolDictPatch >> installSourcePatch [

	" update methodDictionary and update loadedMethod with new compiled method"

	self packageSymbolDictionary addRecompiledMethod: compiledMethod.
	selector := compiledMethod selector
]
