Extension { #name : 'RwGsSymbolDictionaryRegistryV2' }

{ #category : '*rowan-gemstone-loaderv2-36x' }
RwGsSymbolDictionaryRegistryV2 >> adoptCompiledMethod: compiledMethod classExtension: classExtension for: behavior protocol: protocolString toPackageNamed: packageName [
	| methodDictionary selector protocolSymbol loadedMethod loadedPackage loadedClassOrExtension |
	classExtension
		ifTrue: [ 
			"to adopt and extension method, it is much less complicated"
			^ self
				addExtensionCompiledMethod: compiledMethod
				for: behavior
				protocol: protocolString
				toPackageNamed: packageName ].

	methodDictionary := (behavior persistentMethodDictForEnv: 0)
		ifNil: [ Dictionary new ].
	selector := compiledMethod selector.
	compiledMethod
		==
			(methodDictionary
				at: selector
				ifAbsent: [ self error: 'expected an existing compiled method' ])
		ifFalse: [ 
			self
				error:
					'The given compiled method does not the existing compiled method in the class.' ].

	protocolSymbol := protocolString asSymbol.
	(behavior includesCategory: protocolSymbol)
		ifFalse: [ behavior addCategory: protocolSymbol ].
	behavior _moveMethod: selector toCategory: protocolSymbol.

	compiledMethod isFromRowan
		ifTrue: [ 
			"existing LoadedMethod found for compiled method ... ignore already packaged methods"
			^ self ].

	loadedMethod := RwGsLoadedSymbolDictMethod forMethod: compiledMethod.
	compiledMethod _rowanPackageInfo: loadedMethod.

	loadedPackage := self
		loadedPackageNamed: packageName
		ifAbsent: [ 
			self
				error: 'Internal error -- attempt to add a method to a nonexistent package.' ].

	loadedClassOrExtension := loadedPackage
		loadedClassOrClassExtensionForClass: behavior
		ifAbsent: [ 
			self
				error:
					'Internal error -- attempt to add a method to a package in which its class is neither defined nor extended.' ].
	loadedClassOrExtension addLoadedMethod: loadedMethod
]
