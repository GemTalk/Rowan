"
The Rowan-GemStone-Loader implementation does depend upon having a stable and consistent implementation of the RwGsSymbolDictionaryRegistry behavior for the duration of a load.

Right before a load, all of the classes that make up the Rowan-GemStone-Loader package are copied and isolated from the rest of the system, so that updates to the loader code itself will not change the behavior of the loader implementation while the load is in progress.

The class RwGsSymbolDictionaryRegistry is not part of the Rowan-GemStone-Loader package, because instance of the class are expected to have long lifetimes.

This class implements the behavior for RwGsSymbolDictionaryRegistry and since it is the Rowan-GemStone-Loader package, it can be copied and isolated during a load, without affecting the long lived instances of RwGsSymbolDictionaryRegistry.

The methods in the receiver (class-side only) have an extra `instance:` argument tacked on that refers to the instance of RwGsSymbolDictionaryRegistry that was the original target of the message. For example:

	RwGsSymbolDictionaryRegistry_implementation class>>addClassAssociation:forClass:toPackageNamed:instance:

The methods in RwGsSymbolDictionaryRegistry are implemented with a compiled in reference to the RwGsSymbolDictionaryRegistry_implementation class as follows:

	addClassAssociation: assoc forClass: class toPackageNamed: packageName

		^ RwGsSymbolDictionaryRegistry_implementation addClassAssociation: assoc forClass: class toPackageNamed: packageName instance: self

This form is adequate for message sends from classes outside of the Rowan-GemStone-Loader package.

Inside the Rowan-GemStone-Loader package, the messgaes sends must pass in a reference to the `RwGsSymbolDictionaryRegistry_implementation`. This reference will refer to the cloned copy of the class when the RowanLoader symbol dictionary is cloned. The methods have and extra `implementationClass:` that refers to the class and allows the caller to pass in a reference to the cloned class. For example:

	addClassAssociation: assoc forClass: class toPackageNamed: packageName implementationClass: implementationClass

		""Copy the name association to the correct 
			SymbolDictionary in the live SymbolList.
			Create a LoadedClass for the new class, add it to the defining LoadedPackage.""

		^ implementationClass addClassAssociation: assoc forClass: class toPackageNamed: packageName instance: self
"
Class {
	#name : 'RwGsSymbolDictionaryRegistry_ImplementationV2',
	#superclass : 'Object',
	#category : 'Rowan-GemStone-LoaderV2'
}

{ #category : 'private' }
RwGsSymbolDictionaryRegistry_ImplementationV2 class >> _addMovedDeletedMethod: newCompiledMethod  instance: registryInstance [

	"add a recompiled compiled method that was previously removed from loaded things
		to behavior and update the loaded things appropriately"

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

{ #category : 'private' }
RwGsSymbolDictionaryRegistry_ImplementationV2 class >> _clearLookupCachesFor: aBehavior env: envId [

"Invalidates method lookup caches for all classes.
 Invalidates all send-site caches for the specified environment.

 envId must be a SmallInteger >= 0 and <= 255 .
"

 "set bit in VM's cbCodeChanged word so it can propagate to
  other VMs upon commit.  Code which adds or removes entries
  in persistent method dictionaries must send this method. "

	aBehavior 
		_clearLookupCaches: envId;
		_codeChangedForEnv: envId
]

{ #category : 'private' }
RwGsSymbolDictionaryRegistry_ImplementationV2 class >> _doDeleteClassFromLoadedThings: class removeClassFromSystem: removeClassFromSystem instance: registryInstance [

	"a class association is being deleted from the receiver remove it from the loaded things. Conditionally remove class from system"

	self
		_loadedClassFor: class
		noNewVersion: [ :loadedClass | 
			"association for class is present, finish delete operation"
			loadedClass removeFromLoaded: registryInstance.
			self unregisterLoadedClass: loadedClass forClass: class.
			removeClassFromSystem ifTrue: [ registryInstance _symbolDictionary removeKey: loadedClass key asSymbol ] ]
		instance: registryInstance.
	^ registryInstance

]

{ #category : 'method - private api' }
RwGsSymbolDictionaryRegistry_ImplementationV2 class >> _doDeleteCompiledMethod: compiledMethod from: behavior instance: registryInstance [

	"delete a compiled method from behavior"

	| selector |
	selector := compiledMethod selector.
	behavior removeSelector: selector asString environmentId: 0.

	^ registryInstance

]

{ #category : 'method - private api' }
RwGsSymbolDictionaryRegistry_ImplementationV2 class >> _doDeleteCompiledMethodFromLoadedThings: compiledMethod for: behavior instance: registryInstance [

	"remove a compiled method from loaded things"

	| loadedMethod loadedPackage loadedClassOrExtension |
	loadedMethod := registryInstance methodRegistry
		at: compiledMethod
		ifAbsent: [ 
			registryInstance
				error:
					'Internal error -- no existing LoadedMethod found for deleted method.' ].

	registryInstance methodRegistry removeKey: compiledMethod.

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

{ #category : 'class - registration' }
RwGsSymbolDictionaryRegistry_ImplementationV2 class >> _loadedClassExtensionKey [
	^ #rowanLoadedClassExtension
]

{ #category : 'private' }
RwGsSymbolDictionaryRegistry_ImplementationV2 class >> _loadedClassExtensionsFor: class oldClassVersion: oldClass noNewVersion: noNewVersionBlock newVersion: newVersionBlock instance: registryInstance [
	| oldLoadedClassExtensionSet |
	(class isKindOf: Class)
		ifFalse: [ registryInstance error: 'internal error - expected a class' ].

	oldLoadedClassExtensionSet := self
		loadedClassExtensionsForClass: oldClass
		ifAbsent: [ 
			"we're done here"
			^ self ].
	oldLoadedClassExtensionSet copy
		do: [ :oldLoadedClassExtension | 
			| classKey loadedClassExtension loadedPackage |
			loadedPackage := oldLoadedClassExtension loadedPackage.
			self
				unregisterLoadedClassExtension: oldLoadedClassExtension
				forClass: oldClass.
			loadedPackage removeLoadedClassExtension: oldLoadedClassExtension.
			loadedClassExtension := RwGsLoadedSymbolDictClassExtension
				newForClass: class
				inPackage: loadedPackage.
			loadedClassExtension
				reregisterLoadedInstanceMethods:
						oldLoadedClassExtension loadedInstanceMethods;
				reregisterLoadedClassMethods: oldLoadedClassExtension loadedClassMethods;
				yourself.
			self registerLoadedClassExtension: loadedClassExtension forClass: class.
			classKey := loadedClassExtension key asSymbol.
			self
				_symbolDictionary: registryInstance _symbolDictionary
				associationAt: classKey
				ifPresent: [ :assoc | 
					assoc value == loadedClassExtension handle
						ifTrue: [ noNewVersionBlock cull: loadedClassExtension ]
						ifFalse: [ newVersionBlock cull: loadedClassExtension ] ] ]
]

{ #category : 'private' }
RwGsSymbolDictionaryRegistry_ImplementationV2 class >> _loadedClassFor: class noNewVersion: noNewVersionBlock instance: registryInstance [

	self
		_loadedClassFor: class
		noNewVersion: noNewVersionBlock
		newVersion: [ :loadedClass | 
			self
				error:
					'internal error - the value associated with the key ' , class name printString
						, ' is not identical to the given class' ]
		instance: registryInstance.
	^ registryInstance

]

{ #category : 'private' }
RwGsSymbolDictionaryRegistry_ImplementationV2 class >> _loadedClassFor: class noNewVersion: noNewVersionBlock newVersion: newVersionBlock instance: registryInstance [
	(class isKindOf: Class)
		ifFalse: [ registryInstance error: 'internal error - expected a class' ].
	self
		_symbolDictionary: registryInstance _symbolDictionary
		associationAt: class name
		ifPresent: [ :assoc | 
			| loadedClass |
			loadedClass := self
				loadedClassForClass: class
				ifAbsent: [ 
					self
						error:
							'internal error - no loaded class for the class '
								, class name asString printString ].
			assoc value == class
				ifTrue: [ ^ noNewVersionBlock cull: loadedClass cull: assoc ]
				ifFalse: [ ^ newVersionBlock cull: loadedClass cull: assoc ] ].

	registryInstance
		error:
			'internal error - there is no assocation present in the receiver for the given class '
				, class name asString printString
]

{ #category : 'private' }
RwGsSymbolDictionaryRegistry_ImplementationV2 class >> _loadedClassFor: class oldClassVersion: oldClass noNewVersion: noNewVersionBlock newVersion: newVersionBlock instance: registryInstance [
	(class isKindOf: Class)
		ifFalse: [ registryInstance error: 'internal error - expected a class' ].
	self
		_symbolDictionary: registryInstance _symbolDictionary
		associationAt: class name
		ifPresent: [ :assoc | 
			| loadedClass oldLoadedClass loadedPackage |
			oldLoadedClass := self
				loadedClassForClass: oldClass
				ifAbsent: [ 
					registryInstance
						error:
							'internal error - no loaded class found for the old version of '
								, oldClass name asString printString ].
			loadedPackage := oldLoadedClass loadedPackage.
			(self loadedClassForClass: class ifAbsent: [  ])
				ifNil: [ 
					loadedClass := RwGsLoadedSymbolDictClass newForClass: class.
					loadedClass
						reregisterLoadedInstanceMethods: oldLoadedClass loadedInstanceMethods;
						reregisterLoadedClassMethods: oldLoadedClass loadedClassMethods;
						yourself.
					self unregisterLoadedClass: oldLoadedClass forClass: oldClass.
					self registerLoadedClass: loadedClass forClass: class.
					loadedPackage removeLoadedClass: oldLoadedClass.
					loadedPackage addLoadedClass: loadedClass.
					loadedClass updatePropertiesFromClassFor: registryInstance ]
				ifNotNil: [ :unexpectedLoadedClass | 
					assoc value == class
						ifTrue: [ ^ noNewVersionBlock cull: unexpectedLoadedClass cull: assoc ]
						ifFalse: [ 
							registryInstance
								error:
									'internal error - found a loaded class (' , unexpectedLoadedClass printString
										, ') for ' , class name asString printString
										, ' when no loaded class expected' ] ].
			assoc value == class
				ifTrue: [ ^ noNewVersionBlock cull: loadedClass cull: assoc ]
				ifFalse: [ ^ newVersionBlock cull: loadedClass cull: assoc ] ].

	registryInstance
		error:
			'internal error - there is no assocation present in the receiver for the given class '
				, class name asString printString
]

{ #category : 'class - registration' }
RwGsSymbolDictionaryRegistry_ImplementationV2 class >> _loadedClassKey [
	^ #rowanLoadedClass
]

{ #category : 'private' }
RwGsSymbolDictionaryRegistry_ImplementationV2 class >> _symbolDictionary: symbolDictionary associationAt: aKey ifPresent: oneArgBlock [

	"Lookup the given key in the receiver's symbolDictionary. If it is present, answer the value of evaluating the oneArgBlock with the symbol association associated with the key, otherwise answer nil."

	| anAssoc |
	anAssoc := symbolDictionary associationAt: aKey otherwise: nil.
	anAssoc ifNotNil: [ ^ oneArgBlock cull: anAssoc ].
	^ nil

]

{ #category : 'private' }
RwGsSymbolDictionaryRegistry_ImplementationV2 class >> _symbolDictionary: symbolDictionary associationAt: key ifPresent: oneArgBlock ifAbsent: absentBlock [

	"Lookup the given key in the receiver's symbolDictionary. If it is present, answer the value of evaluating the oneArgBlock with the symbol association associated with the key, otherwise answer the value of absentBlock."

	self
		_symbolDictionary: symbolDictionary 
		associationAt: key
		ifPresent: [ :v | ^ oneArgBlock cull: v ].
	^ absentBlock value

]

{ #category : 'private' }
RwGsSymbolDictionaryRegistry_ImplementationV2 class >> _symbolDictionary: symbolDictionary at: key ifPresent: oneArgBlock [

	"Lookup the given key in the receiver's symbolDictionary. If it is present, answer the value of evaluating the given block with the value associated with the key. Otherwise, answer nil."

	| v |
	v := symbolDictionary at: key ifAbsent: [ ^ nil ].
	^ oneArgBlock cull: v

]

{ #category : 'private' }
RwGsSymbolDictionaryRegistry_ImplementationV2 class >> _symbolDictionary: symbolDictionary at: key ifPresent: oneArgBlock ifAbsent: absentBlock [

	"Lookup the given key in the receiver's symbolDictionary. If it is present, answer the value of evaluating the oneArgBlock with the value associated with the key, otherwise answer the value of absentBlock."

	| v |
	v := symbolDictionary at: key ifAbsent: [ ^absentBlock value ].
	^ oneArgBlock cull: v

]

{ #category : 'private' }
RwGsSymbolDictionaryRegistry_ImplementationV2 class >> addClassAssociation: assoc forClass: class toPackageNamed: packageName instance: registryInstance [

	"Copy the name association to the correct 
        SymbolDictionary in the live SymbolList.
        Create a LoadedClass for the new class, add it to the defining LoadedPackage."

	| loadedPackage loadedClass |
	loadedPackage := self existingOrNewLoadedPackageNamed: packageName instance: registryInstance.

	loadedClass := self 
		loadedClassForClass: class 
		ifAbsent: [ 
			loadedClass := RwGsLoadedSymbolDictClass newForClass: class.
			loadedPackage addLoadedClass: loadedClass.
			loadedClass updatePropertiesFromClassFor: registryInstance.
			self registerLoadedClass: loadedClass forClass: class.
			(loadedPackage loadedClassExtensions at: class name ifAbsent: [  ])
				ifNotNil: [ :loadedClassExtension | 
					"I think we need to subsume the loadedClassExtension methods into a loadedClass ..."
					"have yet to come with a test case that takes this path"
					registryInstance error: 'internal error - unexpected class extenstions for a loaded class ', class name asString printString , ' that has no entry in classRegistry in package ', loadedPackage name printString, '.' ].
			loadedClass ].

	self 
		_symbolDictionary: registryInstance _symbolDictionary 
		at: assoc key 
		ifPresent: [:theClass |
			theClass ~~ class
				ifTrue: [
					(RwExistingAssociationWithSameKeyNotification new
						errorMessage: 'Found an existing association in the ',  
								registryInstance _symbolDictionary name asString printString, 
								' symbol dictionary with a duplicate key ',
								assoc key printString, 
								' while attempting to add a new class' )
						signal.
					(registryInstance _symbolDictionary associationAt: assoc key) value: class ] ]
		ifAbsent: [
			assoc value: class.
			registryInstance _symbolDictionary add: assoc].
	^ loadedClass
]

{ #category : 'private' }
RwGsSymbolDictionaryRegistry_ImplementationV2 class >> addExtensionCompiledMethod: compiledMethod for: behavior protocol: protocolString toPackageNamed: packageName instance: registryInstance [
	| methodDictionary selector protocolSymbol existing loadedMethod loadedPackage loadedClassExtension |
	methodDictionary := behavior rwGuaranteePersistentMethodDictForEnv: 0.
	selector := compiledMethod selector.

	(methodDictionary at: selector ifAbsent: [  ])
		ifNotNil: [ :oldCompiledMethod | 
			compiledMethod == oldCompiledMethod
				ifFalse: [ | src oldSrc "temps for ease of debugging" |
					(src := compiledMethod sourceString trimWhiteSpace) = (oldSrc := oldCompiledMethod sourceString trimWhiteSpace)
						ifFalse: [ 
							"only a problem, if the new and old compiled method are not identical"
							self
								error:
									'internal error - Compiled method ' , behavior name asString , '>>'
										, selector asString
										,
											' already exists in method dictionary when new extension method is expected ( package '
										, packageName , ').' ] ] ].

	methodDictionary at: selector put: compiledMethod.
	self _clearLookupCachesFor: behavior env: 0.

	protocolSymbol := protocolString asSymbol.
	(behavior includesCategory: protocolSymbol)
		ifFalse: [ behavior addCategory: protocolSymbol ].
	behavior moveMethod: selector toCategory: protocolSymbol.

	existing := registryInstance methodRegistry
		at: compiledMethod
		ifAbsent: [ nil ].
	existing
		ifNotNil: [ 
			registryInstance
				error:
					'internal error - Existing LoadedMethod ' , behavior name asString , '>>'
						, selector asString , ' found for extension compiled method ( package '
						, packageName , ').' ].
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
			self registerLoadedClassExtension: ext forClass: class.
			ext ].
	loadedClassExtension addLoadedMethod: loadedMethod.
	^ registryInstance
]

{ #category : 'private' }
RwGsSymbolDictionaryRegistry_ImplementationV2 class >> addExtensionSessionMethods: methDict catDict: catDict for: behavior toPackageNamed: packageName instance: registryInstance [


	"expecting only a single method to be in methDict/catDict"

	| dictsArray mDict cDict existing loadedMethod loadedPackage loadedClassExtension compiledMethod |
	dictsArray := registryInstance homeSessionMethods methodAndCategoryDictionaryFor: behavior.
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

	existing := registryInstance methodRegistry at: compiledMethod ifAbsent: [ nil ].
	existing
		ifNotNil: [ 
			registryInstance
				error:
					'Internal error -- existing LoadedMethod found for extension compiled method.' ].
	loadedMethod := RwGsLoadedSymbolDictMethod forMethod: compiledMethod.

	registryInstance methodRegistry at: compiledMethod put: loadedMethod.

	loadedPackage := self existingOrNewLoadedPackageNamed: packageName instance: registryInstance.

	loadedClassExtension := loadedPackage
		loadedClassExtensionForClass: behavior
		ifAbsent: [ 
			| class ext |
			class := behavior theNonMetaClass.
			ext := RwGsLoadedSymbolDictClassExtension
				newForClass: class
				inPackage: loadedPackage.
			self
				registerLoadedClassExtension: ext
				forClass: class.
			ext ].
	loadedClassExtension addLoadedMethod: loadedMethod.

	^ compiledMethod

]

{ #category : 'private' }
RwGsSymbolDictionaryRegistry_ImplementationV2 class >> addMovedDeletedMethod: compiledMethod for: behavior protocol: protocolString toPackageNamed: packageName instance: registryInstance [

	"there is an existing compiled method that has already been deleted from another package ... so we're adding it
		back using specialized processing"

	| methodDictionary selector |
	methodDictionary := (behavior persistentMethodDictForEnv: 0 ) ifNil:[ Dictionary new ].
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

{ #category : 'private' }
RwGsSymbolDictionaryRegistry_ImplementationV2 class >> addNewClassVersionToAssociation: newClass oldClassVersion: oldClass instance: registryInstance [

	"a new class version is being added to the association in the receiver previously occupied by the original class"

	self 
		_loadedClassFor: newClass
		oldClassVersion: oldClass
		noNewVersion: [ 
			"newClass is actually not a new class version of existing class, that's okay... update properties anyway"
			self updateClassProperties: newClass instance: registryInstance.]
		newVersion: [ :loadedClass :assoc | 
			"association for class is present, install in association and update the loadedthing"
			assoc value: newClass.
			loadedClass handleClassDeletionOrNewVersion
				ifTrue: [ loadedClass updatePropertiesFromClassFor: registryInstance ]
				ifFalse: [ registryInstance error: 'internal error - new version of class not properly installed' ] ]
		instance: registryInstance.
	self 
		_loadedClassExtensionsFor: newClass
		oldClassVersion: oldClass
		noNewVersion: [ 
			"newClass is actually not a new class version of existing class, that's okay... noop"
]
		newVersion: [ :loadedClassExtension | 
			loadedClassExtension handleClassDeletionOrNewVersion
				ifFalse: [ registryInstance error: 'internal error - new version of class not properly installed' ] ]
		instance: registryInstance.
]

{ #category : 'private' }
RwGsSymbolDictionaryRegistry_ImplementationV2 class >> addNewCompiledMethod: compiledMethod for: behavior protocol: protocolString toPackageNamed: packageName instance: registryInstance [

	| methodDictionary selector protocolSymbol existing loadedMethod loadedPackage loadedClassOrExtension |
	methodDictionary := behavior rwGuaranteePersistentMethodDictForEnv: 0.
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

{ #category : 'private' }
RwGsSymbolDictionaryRegistry_ImplementationV2 class >> addNewUnmanagedClassVersionToAssociation: newClass oldClassVersion: oldClass toPackageNamed: aPackageName instance: registryInstance [
	"a new class version is being added to the association in the receiver previously occupied by 
		the original class. No loadedClass or loadedClassExtension is expected for the oldClass"

	(oldClass isKindOf: Class)
		ifFalse: [ registryInstance error: 'internal error - expected a class' ].

	(Rowan image loadedClassForClass: oldClass ifAbsent: [  ])
		ifNotNil: [ 
			self
				error:
					'Unexpected loaded class for old version of class '
						, oldClass name asString printString ].

	self
		_symbolDictionary: registryInstance _symbolDictionary
		associationAt: newClass name
		ifPresent: [ :assoc | Rowan packageTools adopt adoptClass: newClass intoPackageNamed: aPackageName ]
]

{ #category : 'private' }
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

{ #category : 'private' }
RwGsSymbolDictionaryRegistry_ImplementationV2 class >> addRecompiledSessionMethodMethod: newCompiledMethod instance: registryInstance [

	"add a recompiled session method compiled method to behavior and update the loaded things"

	| selector behavior dictsArray mDict cDict oldCompiledMethod loadedMethod |
	selector := newCompiledMethod selector.
	behavior := newCompiledMethod inClass.
	dictsArray := registryInstance homeSessionMethods methodAndCategoryDictionaryFor: behavior.
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

	loadedMethod := registryInstance methodRegistry
		at: oldCompiledMethod
		ifAbsent: [ 
			registryInstance
				error:
					'Internal error -- no existing LoadedMethod found for the old compiledMethod.' ].
	registryInstance methodRegistry removeKey: oldCompiledMethod.
	loadedMethod handle: newCompiledMethod.
	registryInstance methodRegistry at: newCompiledMethod put: loadedMethod

]

{ #category : 'package - patch api' }
RwGsSymbolDictionaryRegistry_ImplementationV2 class >> createLoadedPackageFromDefinition: packageDefinition instance: registryInstance [

	^ self
		createLoadedPackageNamed: packageDefinition key
		properties: packageDefinition properties
		instance: registryInstance

]

{ #category : 'package - patch api' }
RwGsSymbolDictionaryRegistry_ImplementationV2 class >> createLoadedPackageNamed: packageName properties: properties instance: registryInstance [ 

	| existingLoadedPackage newLoadedPackage |
	existingLoadedPackage := registryInstance packageRegistry at: packageName ifAbsent: [ nil ].
	existingLoadedPackage
		ifNotNil: [ self error: 'Internal error -- Attempt to add a package that exists.' ].
	newLoadedPackage := RwGsLoadedSymbolDictPackage newNamed: packageName.
	newLoadedPackage setPropertiesTo: properties.
	registryInstance packageRegistry at: packageName put: newLoadedPackage.
	^ newLoadedPackage

]

{ #category : 'class - patch api' }
RwGsSymbolDictionaryRegistry_ImplementationV2 class >> deleteClassFromPackage: class instance: registryInstance [

	"a class association is being deleted from the receiver remove it from the loaded things and remove class from system"

	| removeClassFromSystem |
	removeClassFromSystem := RwDeleteClassFromSystemNotification new
		candidateClass: class;
		signal. "handle and `resume: true` if you want class to remain in system, unpackaged"
	^ self _doDeleteClassFromLoadedThings: class removeClassFromSystem: removeClassFromSystem instance: registryInstance

]

{ #category : 'class - patch api' }
RwGsSymbolDictionaryRegistry_ImplementationV2 class >> deleteClassNamedFromPackage: className instance: registryInstance [

	"a class association is being deleted from the receiver remove it from the loaded things"

	| class |
	class := registryInstance _symbolDictionary
		at: className asSymbol
		ifAbsent: [ registryInstance error: 'No class found for the given class name: ' , className printString ].
	^ self deleteClassFromPackage: class instance: registryInstance

]

{ #category : 'method - patch api' }
RwGsSymbolDictionaryRegistry_ImplementationV2 class >> deleteCompiledMethod: compiledMethod from: behavior instance: registryInstance [

	"delete a compiled method from behavior and remove it from the loaded things"

	self _doDeleteCompiledMethod: compiledMethod from: behavior instance: registryInstance.

	self _doDeleteCompiledMethodFromLoadedThings: compiledMethod for: behavior instance: registryInstance.

	^ registryInstance

]

{ #category : 'method - patch api' }
RwGsSymbolDictionaryRegistry_ImplementationV2 class >> deleteMethod: selector for: behavior instance: registryInstance [

	| compiledMethod |

	compiledMethod := behavior compiledMethodAt: selector.
	self deleteCompiledMethod: compiledMethod from: behavior instance: registryInstance.
	^ registryInstance

]

{ #category : 'method - patch api' }
RwGsSymbolDictionaryRegistry_ImplementationV2 class >> deleteMovedCompiledMethod: compiledMethod from: behavior instance: registryInstance [

	"delete a compiled method from behavior - moved method already removed from the loaded things"

	self _doDeleteCompiledMethod: compiledMethod from: behavior instance: registryInstance.

	^ registryInstance

]

{ #category : 'method - patch api' }
RwGsSymbolDictionaryRegistry_ImplementationV2 class >> deleteMovedLoadedMethod: selector for: behavior instance: registryInstance [

	| compiledMethod |

	compiledMethod := behavior compiledMethodAt: selector.
	self _doDeleteCompiledMethodFromLoadedThings: compiledMethod for: behavior instance: registryInstance.
	^ registryInstance

]

{ #category : 'method - patch api' }
RwGsSymbolDictionaryRegistry_ImplementationV2 class >> deleteMovedMethod: selector for: behavior instance: registryInstance [

	| compiledMethod |

	compiledMethod := behavior compiledMethodAt: selector.
	self deleteMovedCompiledMethod: compiledMethod from: behavior instance: registryInstance.
	^ registryInstance

]

{ #category : 'method - patch api' }
RwGsSymbolDictionaryRegistry_ImplementationV2 class >> deletePackage: packageName instance: registryInstance [

	| loadedPackage |
	loadedPackage := registryInstance packageRegistry
		removeKey: packageName
		ifAbsent: [ registryInstance error: 'package ' , packageName printString , ' not found' ].
	loadedPackage loadedProject removeLoadedPackage: loadedPackage.
	^ registryInstance

]

{ #category : 'class disown api' }
RwGsSymbolDictionaryRegistry_ImplementationV2 class >> disownClass: class instance: registryInstance [

	"Remove the specified class and all of it's packaged instance and class methods from the loaded things."

	self
		_loadedClassFor: class
		noNewVersion: [ :loadedClass | 
			loadedClass disownFromLoaded: registryInstance.
			self 
				unregisterLoadedClass: loadedClass
				forClass: class ]
		instance: registryInstance.
	^ registryInstance

]

{ #category : 'class disown api' }
RwGsSymbolDictionaryRegistry_ImplementationV2 class >> disownClassExtensions: class forLoadedPackage: loadedPackage instance: registryInstance [

	"Remove the extension methods in the class for the given package"

	| loadedClassExtension |
	loadedClassExtension := loadedPackage
		loadedClassExtensionForClass: class
		ifAbsent: [ self error: 'No class extensions found for the class ', class name asString printString, ' in the package ', loadedPackage name printString ].
	
	loadedClassExtension disownFromLoaded: registryInstance.

	loadedPackage removeLoadedClassExtension: loadedClassExtension.

	^ registryInstance

]

{ #category : 'class - patch api' }
RwGsSymbolDictionaryRegistry_ImplementationV2 class >> ensureExtensionClassNamed: className existsForPackageNamed: packageName instance: registryInstance [

	| extensionClass loadedPackage loadedClassExtension |
	extensionClass := registryInstance _symbolDictionary
		at: className asSymbol
		ifAbsent: [ 
			registryInstance
				error:
					'The extension class named ' , className printString
						, ' cannot be found in this symbol dictionary (' , registryInstance _symbolDictionary name printString
						,
							'). Packages that extend a class must be loaded into the symbol dictionary in which the class is installed.' ].
	loadedPackage := self existingOrNewLoadedPackageNamed: packageName instance: registryInstance.

	loadedPackage
		loadedClassExtensionForClass: extensionClass
		ifAbsent: [ 
			loadedClassExtension := RwGsLoadedSymbolDictClassExtension
				newForClass: extensionClass
				inPackage: loadedPackage.
			self
				registerLoadedClassExtension: loadedClassExtension
				forClass: extensionClass ].
	^ registryInstance
]

{ #category : 'package - creation api' }
RwGsSymbolDictionaryRegistry_ImplementationV2 class >> existingOrNewLoadedPackageNamed: packageName instance: registryInstance [

	^ registryInstance packageRegistry
			at: packageName
			ifAbsent: [ 
				registryInstance packageRegistry
					at: packageName
					put: (RwGsLoadedSymbolDictPackage newNamed: packageName) ].

]

{ #category : 'class - registration' }
RwGsSymbolDictionaryRegistry_ImplementationV2 class >> loadedClassExtensionsForClass: aClass [
	^ self
		loadedClassExtensionsForClass: aClass 
		ifAbsent: [ self error: 'No loaded extension class found for: ', aClass name printString]
]

{ #category : 'class - registration' }
RwGsSymbolDictionaryRegistry_ImplementationV2 class >> loadedClassExtensionsForClass: aClass ifAbsent: absentBlock [
	^ (aClass theNonMetaClass theNonMetaClass _extraDictAt: self _loadedClassExtensionKey) ifNil: absentBlock
]

{ #category : 'class - registration' }
RwGsSymbolDictionaryRegistry_ImplementationV2 class >> loadedClassForClass: aClass [
	^ self 
		loadedClassForClass: aClass
		ifAbsent: [ self error: 'No loaded class found for: ', aClass name printString ]
]

{ #category : 'class - registration' }
RwGsSymbolDictionaryRegistry_ImplementationV2 class >> loadedClassForClass: aClass ifAbsent: absentBlock [
	^ (aClass theNonMetaClass _extraDictAt: self _loadedClassKey) ifNil: absentBlock
]

{ #category : 'loaded queries' }
RwGsSymbolDictionaryRegistry_ImplementationV2 class >> loadedHybridPackageNamed: hybridPackageName ifAbsent: absentBlock instance: registryInstance [

	| packageName |
	(hybridPackageName at: 1) = $*
		ifFalse: [ 
			registryInstance
				error:
					'Improperly formed hybrid package name' , hybridPackageName printString
						, '. Expected a leading ''*''' ].
	packageName := (hybridPackageName copyFrom: 2 to: hybridPackageName size)
		asLowercase.
	registryInstance packageRegistry keys
		do: [ :key | 
			key asLowercase = packageName
				ifTrue: [ ^ registryInstance packageRegistry at: key ] ].
	^ absentBlock value

]

{ #category : 'loaded queries' }
RwGsSymbolDictionaryRegistry_ImplementationV2 class >> loadedPackageNamed: packageName ifAbsent: absentBlock instance: registryInstance [

	^ registryInstance packageRegistry at: packageName ifAbsent: absentBlock

]

{ #category : 'class symbol dict - patch api' }
RwGsSymbolDictionaryRegistry_ImplementationV2 class >> moveClassFor: classMove [
	" move class from <oldRegistry> to <newRegistry> which includes moveing the loaded methods associated with the class"

	"loaded things are handled elsewhere, just need to update the various registries ... and move the association"

	"extension methods will be dealt with later"

	| originalSymbolDictionary newSymbolDictionary before assoc theClass loadedClass theBehavior oldRegistry newRegistry |

	before := classMove classBefore.
	originalSymbolDictionary := Rowan image symbolDictNamed: before gs_symbolDictionary.
	assoc := originalSymbolDictionary associationAt: before key asSymbol.
	theClass := assoc value.
	theBehavior := theClass class.
	oldRegistry := originalSymbolDictionary rowanSymbolDictionaryRegistry.

	newSymbolDictionary := Rowan image symbolDictNamed: (classMove classAfter gs_symbolDictionary) .
	newRegistry := newSymbolDictionary rowanSymbolDictionaryRegistry.

	loadedClass := self loadedClassForClass: theClass.
	originalSymbolDictionary removeKey: assoc key.

	self 
		_symbolDictionary: newSymbolDictionary 
		at: assoc key 
		ifPresent: [:class |
			theClass ~~ class
				ifTrue: [ self error: ' internal error - found an existing association in the ',  
								newSymbolDictionary name asString printString, 
								' symbol dictionary with a duplicate key ',
								assoc key printString, 
								' while attempting to move class ', assoc key asString printString ] ]
		ifAbsent: [ newSymbolDictionary add: assoc ].

	loadedClass loadedInstanceMethods values do: [:loadedMethod |
		| compiledMethod |
		compiledMethod := theClass compiledMethodAt: loadedMethod selector asSymbol.
		oldRegistry methodRegistry removeKey: compiledMethod.
		newRegistry methodRegistry at: compiledMethod put: loadedMethod ].
	loadedClass loadedClassMethods values do: [:loadedMethod |
		| compiledMethod |
		compiledMethod := theBehavior compiledMethodAt: loadedMethod selector asSymbol.
		oldRegistry methodRegistry removeKey: compiledMethod.
		newRegistry methodRegistry at: compiledMethod put: loadedMethod ].
]

{ #category : 'method - patch api' }
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

	behavior moveMethod: selector toCategory: newProtocol.

	loadedMethod := registryInstance methodRegistry
		at: compiledMethod
		ifAbsent: [ 
			registryInstance
				error:
					'Internal error -- no existing LoadedMethod found for the compiledMethod.' ].

	loadedMethod updateForProtocolChange.
	^ registryInstance
]

{ #category : 'method - patch api' }
RwGsSymbolDictionaryRegistry_ImplementationV2 class >> movePackage: packageName to: symbolDictionaryName classesWithNewVersions: classesWithNewVersions instance: fromRegistryInstance [
	| loadedPackage toRegistryInstance newClassVersionMap |
	loadedPackage := fromRegistryInstance packageRegistry
		removeKey: packageName
		ifAbsent: [ fromRegistryInstance error: 'package ' , packageName printString , ' not found' ].
	newClassVersionMap := Dictionary new.
	classesWithNewVersions
		do: [ :patch | newClassVersionMap at: patch oldClassVersion put: patch newClassVersion ].
	toRegistryInstance := (Rowan image
		newOrExistingSymbolDictionaryNamed: symbolDictionaryName)
		rowanSymbolDictionaryRegistry.
	loadedPackage loadedClassExtensions
		do: [ :loadedClassExtensions | 
			"class extensions must be removed from old package, additions are handled elsewhere"
			"https://github.com/dalehenrich/Rowan/issues/495"
			loadedClassExtensions moveFrom: fromRegistryInstance.
			newClassVersionMap
				at: loadedClassExtensions handle
				ifPresent: [ :newClassVersion | loadedClassExtensions handle: newClassVersion ].
			loadedClassExtensions moveTo: toRegistryInstance.].
	(toRegistryInstance packageRegistry at: packageName ifAbsent: [  ])
		ifNotNil: [ :unexpectedLoadedPackage | 
			toRegistryInstance
				error:
					'package ' , packageName printString , ' unexpectedly found in '
						, symbolDictionaryName printString ].
	toRegistryInstance packageRegistry at: packageName put: loadedPackage.
	loadedPackage updatePropertiesFromRegistryFor: toRegistryInstance.
	^ fromRegistryInstance
]

{ #category : 'package - creation api' }
RwGsSymbolDictionaryRegistry_ImplementationV2 class >> newLoadedPackageNamed: packageName instance: registryInstance [

	(registryInstance packageRegistry includesKey: packageName)
		ifTrue: [ registryInstance error: 'The package ', packageName printString, ' already exists' ].
	^ registryInstance existingOrNewLoadedPackageNamed: packageName implementationClass: self

]

{ #category : 'class - registration' }
RwGsSymbolDictionaryRegistry_ImplementationV2 class >> registerLoadedClass: loadedClass forClass: aClass [
	aClass theNonMetaClass _extraDictAt: self _loadedClassKey put: loadedClass
]

{ #category : 'class - registration' }
RwGsSymbolDictionaryRegistry_ImplementationV2 class >> registerLoadedClassExtension: loadedClass forClass: aClass [
	((aClass theNonMetaClass _extraDictAt: self _loadedClassExtensionKey) 
		ifNil: [ | set |
			set := IdentitySet new.
			aClass theNonMetaClass _extraDictAt: self _loadedClassExtensionKey put: set.
			set ])
		add: loadedClass
]

{ #category : 'class - registration' }
RwGsSymbolDictionaryRegistry_ImplementationV2 class >> unregisterLoadedClass: loadedClass forClass: aClass [
	| lc |
	(lc := aClass theNonMetaClass _extraDictAt: self _loadedClassKey) == loadedClass
		ifFalse: [ self error: 'Loaded class for ', aClass name printString, ' not found. Found ', lc printString, ' instead.' ].
	aClass theNonMetaClass _extraDictRemoveKey:  self _loadedClassKey
]

{ #category : 'class - registration' }
RwGsSymbolDictionaryRegistry_ImplementationV2 class >> unregisterLoadedClassExtension: loadedClassExtension forClass: aClass [
	(aClass theNonMetaClass _extraDictAt: self _loadedClassExtensionKey)
		ifNil: [ self error: 'Loaded class extension for ', aClass name printString, ' not found' ]
		ifNotNil: [:aSet |
			aSet 
				remove: loadedClassExtension 
				ifAbsent: [ "if the class extension has already been removed we are okay" ].
			aSet isEmpty
				ifTrue: [ aClass theNonMetaClass _extraDictRemoveKey:  self _loadedClassExtensionKey ] ]
]

{ #category : 'class - patch api' }
RwGsSymbolDictionaryRegistry_ImplementationV2 class >> updateClassProperties: class instance: registryInstance [

	"class properties have changed, update the clas properties of the loaded things"

	(class isKindOf: Class)
		ifFalse: [ registryInstance error: 'internal error - expected a class' ].
	registryInstance
		_loadedClassFor: class
		noNewVersion: [ :loadedClass | 
			"association for class is present, update the loaded thing"
			loadedClass updatePropertiesFromClassFor: registryInstance ].
	^ registryInstance

]
