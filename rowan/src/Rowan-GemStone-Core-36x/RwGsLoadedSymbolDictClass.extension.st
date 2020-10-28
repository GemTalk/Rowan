Extension { #name : 'RwGsLoadedSymbolDictClass' }

{ #category : '*rowan-gemstone-core-36x' }
RwGsLoadedSymbolDictClass >> disownFromLoaded: aPackageSymbolDictionary [
	| compiledMethod |
	loadedInstanceMethods
		valuesDo: [ :loadedInstanceMethod | 
			self removeLoadedInstanceMethod: loadedInstanceMethod.
			compiledMethod := loadedInstanceMethod handle.
			compiledMethod _rowanPackageInfo
				ifNotNil: [ :ignoredLoadedMethod | compiledMethod _rowanPackageInfo: nil ]
				ifNil: [ aPackageSymbolDictionary methodRegistry removeKey: compiledMethod ] ].
	loadedClassMethods
		valuesDo: [ :loadedClassMethod | 
			compiledMethod := loadedClassMethod handle.
			compiledMethod _rowanPackageInfo
				ifNotNil: [ :ignoredLoadedMethod | compiledMethod _rowanPackageInfo: nil ]
				ifNil: [ 
					self removeLoadedClassMethod: loadedClassMethod.
					aPackageSymbolDictionary methodRegistry removeKey: compiledMethod ] ]
]
