Extension { #name : 'RwGsLoadedSymbolDictClass' }

{ #category : '*rowan-gemstone-core-36x' }
RwGsLoadedSymbolDictClass >> disownFromLoaded: aPackageSymbolDictionary [
	| compiledMethod |
	loadedInstanceMethods
		valuesDo: [ :loadedInstanceMethod | 
			self removeLoadedInstanceMethod: loadedInstanceMethod.
			compiledMethod := loadedInstanceMethod handle.
			compiledMethod _rowanPackageInfo: nil ].
	loadedClassMethods
		valuesDo: [ :loadedClassMethod | 
			compiledMethod := loadedClassMethod handle.
			compiledMethod _rowanPackageInfo: nil ]
]
