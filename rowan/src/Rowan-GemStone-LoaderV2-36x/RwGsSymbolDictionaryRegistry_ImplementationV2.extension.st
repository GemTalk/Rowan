Extension { #name : 'RwGsSymbolDictionaryRegistry_ImplementationV2' }

{ #category : '*rowan-gemstone-loaderv2-36x' }
RwGsSymbolDictionaryRegistry_ImplementationV2 class >> _doDeleteCompiledMethodFromLoadedThings: compiledMethod for: behavior instance: registryInstance [
	"remove a compiled method from loaded things"

	| loadedMethod loadedPackage loadedClassOrExtension |
	compiledMethod _rowanPackageInfo
		ifNotNil: [ :aLoadedMethod | 
			compiledMethod _rowanPackageInfo: nil.
			loadedMethod := aLoadedMethod ]
		ifNil: [ 
			registryInstance
				error:
					'Internal error -- no existing LoadedMethod found for deleted method ('
						, behavior printString , '>>' , compiledMethod selector , ').' ].

	loadedPackage := loadedMethod loadedPackage.
	loadedClassOrExtension := loadedPackage
		classOrExtensionForClass: behavior
		ifAbsent: [ 
			registryInstance
				error:
					'Internal error -- attempt to remove a method (' , behavior printString , '>>'
						, compiledMethod selector
						, ' ) from a package in which its class is neither defined nor extended.' ].

	loadedClassOrExtension removeLoadedMethod: loadedMethod.
	loadedClassOrExtension isLoadedClassExtension
		ifTrue: [ 
			loadedClassOrExtension isEmpty
				ifTrue: [ 
					self
						unregisterLoadedClassExtension: loadedClassOrExtension
						forClass: loadedClassOrExtension handle.
					loadedPackage removeLoadedClassExtension: loadedClassOrExtension ] ].

	^ registryInstance
]

{ #category : '*rowan-gemstone-loaderv2-36x' }
RwGsSymbolDictionaryRegistry_ImplementationV2 class >> addExtensionCompiledMethod: compiledMethod for: behavior protocol: protocolString toPackageNamed: packageName instance: registryInstance [
	| methodDictionary selector protocolSymbol existing loadedMethod loadedPackage loadedClassExtension |
	methodDictionary := behavior rwGuaranteePersistentMethodDictForEnv: 0.
	selector := compiledMethod selector.

	methodDictionary at: selector put: compiledMethod.
	self _clearLookupCachesFor: behavior env: 0.

	protocolSymbol := protocolString asSymbol.
	(behavior includesCategory: protocolSymbol)
		ifFalse: [ behavior addCategory: protocolSymbol ].
	behavior _moveMethod: selector toCategory: protocolSymbol.

	existing := compiledMethod _rowanPackageInfo.
	existing
		ifNotNil: [ 
			registryInstance
				error:
					'internal error - Existing LoadedMethod ' , behavior name asString , '>>'
						, selector asString , ' found for extension compiled method ( package '
						, packageName , ').' ].
	loadedMethod := RwGsLoadedSymbolDictMethod forMethod: compiledMethod.

	compiledMethod _rowanPackageInfo: loadedMethod.

	loadedPackage := Rowan image
		loadedPackageNamed: packageName
		ifAbsent: [ self existingOrNewLoadedPackageNamed: packageName instance: registryInstance ].

	loadedClassExtension := loadedPackage
		loadedClassExtensionForClass: behavior
		ifAbsent: [ 
			| class ext |
			class := behavior theNonMetaClass.
			ext := RwGsLoadedSymbolDictClassExtension
				newForClass: class
				inPackage: loadedPackage.
			self registerLoadedClassExtension: ext forClass: class.
			ext ].
	loadedClassExtension addLoadedMethod: loadedMethod.
	^ registryInstance
]

{ #category : '*rowan-gemstone-loaderv2-36x' }
RwGsSymbolDictionaryRegistry_ImplementationV2 class >> addExtensionSessionMethods: methDict catDict: catDict for: behavior toPackageNamed: packageName instance: registryInstance [
	"expecting only a single method to be in methDict/catDict"

	| dictsArray mDict cDict existing loadedMethod loadedPackage loadedClassExtension compiledMethod |
	dictsArray := registryInstance homeSessionMethods
		methodAndCategoryDictionaryFor: behavior.
	mDict := dictsArray at: 1.
	cDict := dictsArray at: 2.
	catDict
		keysAndValuesDo: [ :protocolSymbol :selectors | 
			| symbolSet |
			symbolSet := cDict
				at: protocolSymbol
				ifAbsent: [ cDict at: protocolSymbol put: SymbolSet new ].
			symbolSet addAll: selectors ].
	methDict
		keysAndValuesDo: [ :sel :meth | 
			mDict at: sel put: meth.
			GsPackagePolicy current updateMethodLookupCacheFor: meth in: behavior.
			compiledMethod := meth ].

	existing := compiledMethod _rowanPackageInfo.
	existing
		ifNotNil: [ 
			registryInstance
				error:
					'Internal error -- existing LoadedMethod found for extension compiled method.' ].
	loadedMethod := RwGsLoadedSymbolDictMethod forMethod: compiledMethod.

	compiledMethod _rowanPackageInfo: loadedMethod.

	loadedPackage := self
		existingOrNewLoadedPackageNamed: packageName
		instance: registryInstance.

	loadedClassExtension := loadedPackage
		loadedClassExtensionForClass: behavior
		ifAbsent: [ 
			| class ext |
			class := behavior theNonMetaClass.
			ext := RwGsLoadedSymbolDictClassExtension
				newForClass: class
				inPackage: loadedPackage.
			self registerLoadedClassExtension: ext forClass: class.
			ext ].
	loadedClassExtension addLoadedMethod: loadedMethod.

	^ compiledMethod
]

{ #category : '*rowan-gemstone-loaderv2-36x' }
RwGsSymbolDictionaryRegistry_ImplementationV2 class >> addNewCompiledMethod: compiledMethod for: behavior protocol: protocolString toPackageNamed: packageName instance: registryInstance [
	| methodDictionary selector protocolSymbol existing loadedMethod loadedPackage loadedClassOrExtension |
	methodDictionary := behavior rwGuaranteePersistentMethodDictForEnv: 0.
	selector := compiledMethod selector.
	(methodDictionary at: selector ifAbsent: [  ])
		ifNotNil: [ :oldCompiledMethod | 
			"there is an existing compiled method ... that means we're adding a recompiled methoded and moving it to the (possibly new) protocol"
			oldCompiledMethod _rowanPackageInfo
				ifNotNil: [ 
					self addRecompiledMethod: compiledMethod instance: registryInstance.
					^ self
						moveCompiledMethod: compiledMethod
						toProtocol: protocolString
						instance: registryInstance ] ].
	methodDictionary at: selector put: compiledMethod.
	self _clearLookupCachesFor: behavior env: 0.

	protocolSymbol := protocolString asSymbol.
	(behavior includesCategory: protocolSymbol)
		ifFalse: [ behavior addCategory: protocolSymbol ].
	behavior _moveMethod: selector toCategory: protocolSymbol.

	existing := compiledMethod _rowanPackageInfo.
	existing
		ifNotNil: [ 
			registryInstance
				error: 'Internal error -- existing LoadedMethod found for compiled method.' ].
	loadedMethod := RwGsLoadedSymbolDictMethod forMethod: compiledMethod.

	compiledMethod _rowanPackageInfo: loadedMethod.

	loadedPackage := self
		loadedPackageNamed: packageName
		ifAbsent: [ 
			registryInstance
				error: 'Internal error -- attempt to add a method to a nonexistent package.' ]
		instance: registryInstance.

	loadedClassOrExtension := loadedPackage
		loadedClassOrClassExtensionForClass: behavior
		ifAbsent: [ 
			registryInstance
				error:
					'Internal error -- attempt to add a method to a package in which its class is neither defined nor extended.' ].
	loadedClassOrExtension addLoadedMethod: loadedMethod.
	^ registryInstance
]

{ #category : '*rowan-gemstone-loaderv2-36x' }
RwGsSymbolDictionaryRegistry_ImplementationV2 class >> addRecompiledMethod: newCompiledMethod instance: registryInstance [
	"add a recompiled compiled method to behavior and update the loaded things"

	| selector behavior methodDictionary oldCompiledMethod loadedMethod |
	selector := newCompiledMethod selector.
	behavior := newCompiledMethod inClass.
	methodDictionary := behavior rwGuaranteePersistentMethodDictForEnv: 0.
	oldCompiledMethod := methodDictionary
		at: selector
		ifAbsent: [ 
			registryInstance
				error:
					'Internal error -- expected an existing compiled method ('
						, behavior printString , '>>' , selector , ') in the method dictionary' ].

	oldCompiledMethod == newCompiledMethod
		ifTrue: [ 
			"exit early, no more work to be done"
			^ registryInstance ].
	methodDictionary at: selector put: newCompiledMethod.
	self _clearLookupCachesFor: behavior env: 0.

	loadedMethod := oldCompiledMethod _rowanPackageInfo.
	loadedMethod
		ifNil: [ 
			registryInstance
				error:
					'Internal error -- no existing LoadedMethod found for the old compiledMethod  ('
						, behavior printString , '>>' , selector , ').' ].
	oldCompiledMethod _rowanPackageInfo: nil.

	loadedMethod handle: newCompiledMethod.
	newCompiledMethod _rowanPackageInfo: loadedMethod.
	^ registryInstance
]

{ #category : '*rowan-gemstone-loaderv2-36x' }
RwGsSymbolDictionaryRegistry_ImplementationV2 class >> addRecompiledSessionMethodMethod: newCompiledMethod instance: registryInstance [
	"add a recompiled session method compiled method to behavior and update the loaded things"

	| selector behavior dictsArray mDict cDict oldCompiledMethod loadedMethod |
	selector := newCompiledMethod selector.
	behavior := newCompiledMethod inClass.
	dictsArray := registryInstance homeSessionMethods
		methodAndCategoryDictionaryFor: behavior.
	mDict := dictsArray at: 1.
	cDict := dictsArray at: 2.
	oldCompiledMethod := mDict
		at: selector
		ifAbsent: [ 
			registryInstance
				error:
					'Internal error -- expected an existing compiled method  ('
						, behavior printString , '>>' , selector
						, ') in the session method method dictionary' ].

	oldCompiledMethod == newCompiledMethod
		ifTrue: [ 
			"exit early, no more work to be done"
			^ registryInstance ].
	mDict at: selector put: newCompiledMethod.
	GsPackagePolicy current
		updateMethodLookupCacheFor: newCompiledMethod
		in: behavior.

	loadedMethod := oldCompiledMethod _rowanPackageInfo.
	loadedMethod
		ifNil: [ 
			registryInstance
				error:
					'Internal error -- no existing LoadedMethod found for the old compiledMethod ('
						, behavior printString , '>>' , selector , ').' ].
	oldCompiledMethod _rowanPackageInfo: nil.

	loadedMethod handle: newCompiledMethod.
	newCompiledMethod _rowanPackageInfo: loadedMethod
]

{ #category : '*rowan-gemstone-loaderv2-36x' }
RwGsSymbolDictionaryRegistry_ImplementationV2 class >> moveClassFor: classMove [
	" move class from <oldRegistry> to <newRegistry> which includes moveing the loaded methods associated with the class"

	"loaded things are handled elsewhere, just need to update the various registries ... and move the association"

	"extension methods will be dealt with later"

	| originalSymbolDictionary newSymbolDictionary before assoc theClass loadedClass theBehavior oldRegistry newRegistry |
	before := classMove classBefore.
	originalSymbolDictionary := Rowan image
		symbolDictNamed: before gs_symbolDictionary.
	assoc := originalSymbolDictionary associationAt: before key asSymbol.
	theClass := assoc value.
	theBehavior := theClass class.
	oldRegistry := originalSymbolDictionary rowanSymbolDictionaryRegistry.

	newSymbolDictionary := Rowan image
		symbolDictNamed: classMove classAfter gs_symbolDictionary.
	newRegistry := newSymbolDictionary rowanSymbolDictionaryRegistry.

	loadedClass := self loadedClassForClass: theClass.
	originalSymbolDictionary removeKey: assoc key.

	self
		_symbolDictionary: newSymbolDictionary
		at: assoc key
		ifPresent: [ :class | 
			theClass ~~ class
				ifTrue: [ 
					self
						error:
							' internal error - found an existing association in the '
								, newSymbolDictionary name asString printString
								, ' symbol dictionary with a duplicate key ' , assoc key printString
								, ' while attempting to move class ' , assoc key asString printString ] ]
		ifAbsent: [ newSymbolDictionary add: assoc ].

	loadedClass loadedInstanceMethods values
		do: [ :loadedMethod | 
			| compiledMethod |
			"this operation can probably be skipped, since the loaded method itself is not changing"
			compiledMethod := theClass compiledMethodAt: loadedMethod selector asSymbol.
			compiledMethod _rowanPackageInfo: loadedMethod ].
	loadedClass loadedClassMethods values
		do: [ :loadedMethod | 
			| compiledMethod |
			"this operation can probably be skipped, since the loaded method itself is not changing"
			compiledMethod := theBehavior
				compiledMethodAt: loadedMethod selector asSymbol.
			compiledMethod _rowanPackageInfo: loadedMethod ]
]

{ #category : '*rowan-gemstone-loaderv2-36x' }
RwGsSymbolDictionaryRegistry_ImplementationV2 class >> moveCompiledMethod: compiledMethod toProtocol: newProtocol instance: registryInstance [
	"move a compiled method into a different protocol and update loaded things"

	| behavior selector loadedMethod oldCat catSym catDict methodDictionary existingCompiledMethod |
	selector := compiledMethod selector.
	behavior := compiledMethod inClass.

	methodDictionary := (behavior persistentMethodDictForEnv: 0)
		ifNil: [ Dictionary new ].
	existingCompiledMethod := methodDictionary
		at: selector
		ifAbsent: [ 
			registryInstance
				error:
					'Internal error -- no existing CompileMethod found for patched method ('
						, behavior printString , '>>' , selector, ')' ].
	existingCompiledMethod == compiledMethod
		ifFalse: [ 
			registryInstance
				error:
					'Internal error - the existingCompiledMethod (' , behavior printString , '>>'
						, selector , ') is not identical to the compiled method arg' ].

	oldCat := behavior categoryOfSelector: selector environmentId: 0.
	catSym := newProtocol asSymbol.
	catDict := behavior _baseCategorysForStore: 0.
	oldCat ifNotNil: [ (catDict at: oldCat) remove: selector ].
	catDict
		at: catSym
		ifAbsent: [ behavior addCategory: newProtocol environmentId: 0 ].
	(catDict at: catSym) add: selector.

	behavior _moveMethod: selector toCategory: newProtocol.

	loadedMethod := compiledMethod _rowanPackageInfo.
	loadedMethod
		ifNil: [ 
			registryInstance
				error:
					'Internal error -- no existing LoadedMethod found for the compiledMethod ('
						, behavior printString , '>>' , selector , ').' ].

	loadedMethod updateForProtocolChange.
	^ registryInstance
]
