Extension { #name : 'RwGsSymbolDictionaryRegistry_Implementation' }

{ #category : '*rowan-gemstone-loader-32-5' }
RwGsSymbolDictionaryRegistry_Implementation class >> _addMovedDeletedMethod: newCompiledMethod  instance: registryInstance [

	"add a recompiled compiled method that was previously removed from loaded things
		to behavior and update the loaded things appropriately"

	| selector behavior methodDictionary oldCompiledMethod loadedMethod |
	selector := newCompiledMethod selector.
	behavior := newCompiledMethod inClass.
	methodDictionary := behavior persistentMethodDictForEnv: 0.
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

	loadedMethod := registryInstance methodRegistry
		at: oldCompiledMethod
		ifAbsent: [].
	loadedMethod ifNotNil: [  
			registryInstance
				error:
					'Internal error -- unexpected loaded method found - deleteMethod processing should have removed the loaded method already' ].

	loadedMethod := RwGsLoadedSymbolDictMethod forMethod: newCompiledMethod.

	registryInstance methodRegistry at: newCompiledMethod put: loadedMethod.
	^ registryInstance
]

{ #category : '*rowan-gemstone-loader-32-5' }
RwGsSymbolDictionaryRegistry_Implementation class >> addExtensionCompiledMethod: compiledMethod for: behavior protocol: protocolString toPackageNamed: packageName instance: registryInstance [

	| methodDictionary selector protocolSymbol existing loadedMethod loadedPackage loadedClassExtension |
	methodDictionary := behavior persistentMethodDictForEnv: 0.
	selector := compiledMethod selector.
	methodDictionary at: selector put: compiledMethod.
	self _clearLookupCachesFor: behavior env: 0.

	protocolSymbol := protocolString asSymbol.
	(behavior includesCategory: protocolSymbol)
		ifFalse: [ behavior addCategory: protocolSymbol ].
	behavior moveMethod: selector toCategory: protocolSymbol.

	existing := registryInstance methodRegistry at: compiledMethod ifAbsent: [ nil ].
	existing
		ifNotNil: [ 
			registryInstance
				error:
					'Internal error -- existing LoadedMethod found for extension compiled method.' ].
	loadedMethod := RwGsLoadedSymbolDictMethod forMethod: compiledMethod.

	registryInstance methodRegistry at: compiledMethod put: loadedMethod.

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
			(registryInstance classExtensionRegistry
				at: class classHistory
				ifAbsentPut: [ IdentitySet new ]) add: ext.
			ext ].
	loadedClassExtension addLoadedMethod: loadedMethod.
	^ registryInstance
]

{ #category : '*rowan-gemstone-loader-32-5' }
RwGsSymbolDictionaryRegistry_Implementation class >> addMovedDeletedMethod: compiledMethod for: behavior protocol: protocolString toPackageNamed: packageName instance: registryInstance [

	"there is an existing compiled method that has already been deleted from another package ... so we're adding it
		back using specialized processing"

	| methodDictionary selector |
	methodDictionary := behavior persistentMethodDictForEnv: 0.
	selector := compiledMethod selector.
	methodDictionary 
		at: selector 
		ifAbsent: [  
			registryInstance
				error:
					'Internal error -- attempt to move a method that does not exist'  ].
	self _addMovedDeletedMethod: compiledMethod instance: registryInstance.
	^ self moveCompiledMethod: compiledMethod toProtocol: protocolString instance: registryInstance

]

{ #category : '*rowan-gemstone-loader-32-5' }
RwGsSymbolDictionaryRegistry_Implementation class >> addNewCompiledMethod: compiledMethod for: behavior protocol: protocolString toPackageNamed: packageName instance: registryInstance [

	| methodDictionary selector protocolSymbol existing loadedMethod loadedPackage loadedClassOrExtension |
	methodDictionary := behavior persistentMethodDictForEnv: 0.
	selector := compiledMethod selector.
	(methodDictionary at: selector ifAbsent: [  ])
		ifNotNil: [ :oldCompiledMethod | 
			"there is an existing compiled method ... that means we're adding a recompiled methoded and moving it to the (possibly new) protocol"
			self addRecompiledMethod: compiledMethod instance: registryInstance.
			^ self 
				moveCompiledMethod: compiledMethod toProtocol: protocolString instance: registryInstance ].
	methodDictionary at: selector put: compiledMethod.
	self _clearLookupCachesFor: behavior env: 0.

	protocolSymbol := protocolString asSymbol.
	(behavior includesCategory: protocolSymbol)
		ifFalse: [ behavior addCategory: protocolSymbol ].
	behavior moveMethod: selector toCategory: protocolSymbol.

	existing := registryInstance methodRegistry at: compiledMethod ifAbsent: [ nil ].
	existing
		ifNotNil: [ registryInstance error: 'Internal error -- existing LoadedMethod found for compiled method.' ].
	loadedMethod := RwGsLoadedSymbolDictMethod forMethod: compiledMethod.

	registryInstance methodRegistry at: compiledMethod put: loadedMethod.

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

{ #category : '*rowan-gemstone-loader-32-5' }
RwGsSymbolDictionaryRegistry_Implementation class >> addRecompiledMethod: newCompiledMethod instance: registryInstance [

	"add a recompiled compiled method to behavior and update the loaded things"

	| selector behavior methodDictionary oldCompiledMethod loadedMethod |
	selector := newCompiledMethod selector.
	behavior := newCompiledMethod inClass.
	methodDictionary := behavior persistentMethodDictForEnv: 0.
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

	loadedMethod := registryInstance methodRegistry
		at: oldCompiledMethod
		ifAbsent: [ 
			registryInstance
				error:
					'Internal error -- no existing LoadedMethod found for the old compiledMethod.' ].
	registryInstance methodRegistry removeKey: oldCompiledMethod.
	loadedMethod handle: newCompiledMethod.
	registryInstance methodRegistry at: newCompiledMethod put: loadedMethod.
	^ registryInstance
]

{ #category : '*rowan-gemstone-loader-32-5' }
RwGsSymbolDictionaryRegistry_Implementation class >> moveCompiledMethod: compiledMethod toProtocol: newProtocol instance: registryInstance [

	"move a compiled method into a different protocol and update loaded things"

	| behavior selector loadedMethod oldCat catSym catDict methodDictionary existingCompiledMethod |
	selector := compiledMethod selector.
	behavior := compiledMethod inClass.

	methodDictionary := behavior persistentMethodDictForEnv: 0.
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

	behavior moveMethod: selector toCategory: newProtocol environmentId: 0.

	loadedMethod := registryInstance methodRegistry
		at: compiledMethod
		ifAbsent: [ 
			registryInstance
				error:
					'Internal error -- no existing LoadedMethod found for the compiledMethod.' ].

	loadedMethod updateForProtocolChange.
	^ registryInstance

]
