Extension { #name : 'RwGsSymbolDictionaryRegistry_ImplementationV2' }

{ #category : '*rowan-gemstone-loaderv2-36x' }
RwGsSymbolDictionaryRegistry_ImplementationV2 class >> _doDeleteCompiledMethodFromLoadedThings: compiledMethod for: behavior instance: registryInstance [
	"remove a compiled method from loaded things"

	| loadedMethod loadedPackage loadedClassOrExtension |
	compiledMethod _origin
		ifNotNil: [ :aLoadedMethod | 
			compiledMethod _origin: nil.
			loadedMethod := aLoadedMethod ]
		ifNil: [ 
			registryInstance
				error:
					'Internal error -- no existing LoadedMethod found for deleted method.' ].

	loadedPackage := loadedMethod loadedPackage.
	loadedClassOrExtension := loadedPackage
		classOrExtensionForClass: behavior
		ifAbsent: [ 
			registryInstance
				error:
					'Internal error -- attempt to remove a method from a package in which its class is neither defined nor extended.' ].

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
	| methodDictionary selector protocolSymbol loadedMethod loadedPackage loadedClassExtension |
	methodDictionary := behavior rwGuaranteePersistentMethodDictForEnv: 0.
	selector := compiledMethod selector.

	methodDictionary at: selector put: compiledMethod.
	self _clearLookupCachesFor: behavior env: 0.

	protocolSymbol := protocolString asSymbol.
	(behavior includesCategory: protocolSymbol)
		ifFalse: [ behavior addCategory: protocolSymbol ].
	behavior _moveMethod: selector toCategory: protocolSymbol.

	compiledMethod isFromRowan
		ifTrue: [ 
			registryInstance
				error:
					'internal error - Existing LoadedMethod ' , behavior name asString , '>>'
						, selector asString , ' found for extension compiled method ( package '
						, packageName , ').' ].
	loadedMethod := RwGsLoadedSymbolDictMethod forMethod: compiledMethod.

	compiledMethod _origin: loadedMethod.

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

	existing := compiledMethod _origin.
	existing
		ifNotNil: [ 
			registryInstance
				error:
					'Internal error -- existing LoadedMethod found for extension compiled method.' ].
	loadedMethod := RwGsLoadedSymbolDictMethod forMethod: compiledMethod.

	compiledMethod _origin: loadedMethod.

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
			self addRecompiledMethod: compiledMethod instance: registryInstance.
			^ self
				moveCompiledMethod: compiledMethod
				toProtocol: protocolString
				instance: registryInstance ].
	methodDictionary at: selector put: compiledMethod.
	self _clearLookupCachesFor: behavior env: 0.

	protocolSymbol := protocolString asSymbol.
	(behavior includesCategory: protocolSymbol)
		ifFalse: [ behavior addCategory: protocolSymbol ].
	behavior _moveMethod: selector toCategory: protocolSymbol.

	existing := compiledMethod _origin.
	existing
		ifNotNil: [ 
			registryInstance
				error: 'Internal error -- existing LoadedMethod found for compiled method.' ].
	loadedMethod := RwGsLoadedSymbolDictMethod forMethod: compiledMethod.

	compiledMethod _origin: loadedMethod.

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
					'Internal error -- expected an existing compiled method in the method dictionary' ].

	oldCompiledMethod == newCompiledMethod
		ifTrue: [ 
			"exit early, no more work to be done"
			^ registryInstance ].
	methodDictionary at: selector put: newCompiledMethod.
	self _clearLookupCachesFor: behavior env: 0.

	loadedMethod := oldCompiledMethod _origin.
	loadedMethod
		ifNil: [ 
			registryInstance
				error:
					'Internal error -- no existing LoadedMethod found for the old compiledMethod.' ].
	oldCompiledMethod _origin: nil.

	loadedMethod handle: newCompiledMethod.
	newCompiledMethod _origin: loadedMethod.
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
					'Internal error -- expected an existing compiled method in the session method method dictionary' ].

	oldCompiledMethod == newCompiledMethod
		ifTrue: [ 
			"exit early, no more work to be done"
			^ registryInstance ].
	mDict at: selector put: newCompiledMethod.
	GsPackagePolicy current
		updateMethodLookupCacheFor: newCompiledMethod
		in: behavior.

	loadedMethod := oldCompiledMethod _origin.
	loadedMethod
		ifNil: [ 
			registryInstance
				error:
					'Internal error -- no existing LoadedMethod found for the old compiledMethod.' ].
	oldCompiledMethod _origin: nil.

	loadedMethod handle: newCompiledMethod.
	newCompiledMethod _origin: loadedMethod
]

{ #category : '*rowan-gemstone-loaderv2-36x' }
RwGsSymbolDictionaryRegistry_ImplementationV2 class >> moveClassFor: classMove [
	" move class from <originalSymbolDictionary> to <newSymbolDictionary> "

	"loaded things are handled elsewhere, just need to move the association"

	| originalSymbolDictionary newSymbolDictionary before after assoc theClass loadedClass theBehavior |
	before := classMove classBefore.
	after := classMove classAfter.
	originalSymbolDictionary := Rowan image
		symbolDictNamed: before gs_symbolDictionary.
	newSymbolDictionary := Rowan image symbolDictNamed: after gs_symbolDictionary.

	assoc := originalSymbolDictionary
		associationAt: before key asSymbol
		ifAbsent: [ 
			assoc := newSymbolDictionary
				associationAt: after key asSymbol
				ifAbsent: [ 
					self
						error:
							'internal error - no association found for the class ' , before name printString
								, ' in either of the expected symbol dictionaries ('
								, before gs_symbolDictionary asString , ' or '
								, after gs_symbolDictionary asString , ').' ].
			^ self	"class has already been moved (perhaps new version logic), we're done" ].
	theClass := assoc value.
	theBehavior := theClass class.

	newSymbolDictionary := Rowan image symbolDictNamed: after gs_symbolDictionary.

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
		ifAbsent: [ newSymbolDictionary add: assoc ]
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
					'Internal error -- no existing CompileMethod found for patched method.' ].
	existingCompiledMethod == compiledMethod
		ifFalse: [ 
			registryInstance
				error:
					'Internal error - the existingCompiledMethod is not identical to the compiled method arg' ].

	oldCat := behavior categoryOfSelector: selector environmentId: 0.
	catSym := newProtocol asSymbol.
	catDict := behavior _baseCategorysForStore: 0.
	oldCat ifNotNil: [ (catDict at: oldCat) remove: selector ].
	catDict
		at: catSym
		ifAbsent: [ behavior addCategory: newProtocol environmentId: 0 ].
	(catDict at: catSym) add: selector.

	behavior _moveMethod: selector toCategory: newProtocol.

	loadedMethod := compiledMethod _origin.
	loadedMethod
		ifNil: [ 
			registryInstance
				error:
					'Internal error -- no existing LoadedMethod found for the compiledMethod.' ].

	loadedMethod updateForProtocolChange.
	^ registryInstance
]
