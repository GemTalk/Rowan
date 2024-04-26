Extension { #name : 'RwGsLoadedSymbolDictClass' }

{ #category : '*rowan-gemstone-core-36x' }
RwGsLoadedSymbolDictClass >> disownFromLoaded: aPackageSymbolDictionary [
	| compiledMethod |
	loadedInstanceMethods
		valuesDo: [ :loadedInstanceMethod | 
			self removeLoadedInstanceMethod: loadedInstanceMethod.
			compiledMethod := loadedInstanceMethod handle.
			compiledMethod _origin: nil ].
	loadedClassMethods
		valuesDo: [ :loadedClassMethod | 
			compiledMethod := loadedClassMethod handle.
			compiledMethod _origin: nil ]
]
