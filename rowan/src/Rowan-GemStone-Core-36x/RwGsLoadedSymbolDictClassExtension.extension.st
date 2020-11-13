Extension { #name : 'RwGsLoadedSymbolDictClassExtension' }

{ #category : '*rowan-gemstone-core-36x' }
RwGsLoadedSymbolDictClassExtension >> disownFromLoaded: registry [
	| compiledMethod |
	loadedInstanceMethods
		valuesDo: [ :loadedInstanceMethod | 
			self removeLoadedInstanceMethod: loadedInstanceMethod.
			compiledMethod := loadedInstanceMethod handle.
			compiledMethod _rowanPackageInfo: nil ].
	loadedClassMethods
		valuesDo: [ :loadedClassMethod | 
			self removeLoadedClassMethod: loadedClassMethod.
			compiledMethod := loadedClassMethod handle.
			compiledMethod _rowanPackageInfo: nil ].

	registry unregisterLoadedClassExtension: self forClass: handle
]
