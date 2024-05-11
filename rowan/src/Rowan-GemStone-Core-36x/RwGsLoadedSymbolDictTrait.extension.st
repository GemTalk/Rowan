Extension { #name : 'RwGsLoadedSymbolDictTrait' }

{ #category : '*rowan-gemstone-core-36x' }
RwGsLoadedSymbolDictTrait >> disownFromLoaded: registry [
	loadedInstanceMethods
		valuesDo: [ :loadedInstanceMethod | self removeLoadedInstanceMethod: loadedInstanceMethod ].
	loadedClassMethods
		valuesDo: [ :loadedClassMethod | self removeLoadedClassMethod: loadedClassMethod ].

	registry unregisterLoadedTrait: self forTrait: handle
]
