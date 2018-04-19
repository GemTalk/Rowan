Class {
	#name : 'RwGsSymbolDictionaryRegistry',
	#superclass : 'Object',
	#instVars : [
		'symbolDictionary',
		'packageRegistry',
		'classRegistry',
		'classExtensionRegistry',
		'methodRegistry'
	],
	#category : 'Rowan-GemStone'
}

{ #category : 'accessing' }
RwGsSymbolDictionaryRegistry class >> _classAdditionPatchClass [

	^ RwGsClassAdditionSymbolDictPatch
]

{ #category : 'accessing' }
RwGsSymbolDictionaryRegistry class >> _classDeletionPatchClass [

	^ RwGsClassDeletionSymbolDictPatch 
]

{ #category : 'accessing' }
RwGsSymbolDictionaryRegistry class >> _classExtensionPatchClass [

	^ RwGsClassExtensionSymbolDictPatch
]

{ #category : 'accessing' }
RwGsSymbolDictionaryRegistry class >> _classPropertiesPatchClass [

	^ RwGsClassPropertiesSymDictPatch
]

{ #category : 'accessing' }
RwGsSymbolDictionaryRegistry class >> _classVariablePatchClass [

	^ RwGsClassVariableChangeSymbolDictPatch
]

{ #category : 'accessing' }
RwGsSymbolDictionaryRegistry class >> _classVersioningPatchClass [

	^ RwGsClassVersioningSymbolDictPatch
]

{ #category : 'accessing' }
RwGsSymbolDictionaryRegistry class >> _methodAdditionPatchClass [

	^ RwGsMethodAdditionSymbolDictPatch
]

{ #category : 'accessing' }
RwGsSymbolDictionaryRegistry class >> _methodDeletionPatchClass [

	^ RwGsMethodDeletionSymbolDictPatch
]

{ #category : 'accessing' }
RwGsSymbolDictionaryRegistry class >> _methodExtensionDeletionPatchClass [

	^ RwGsMethodDeletionExtensionSymbolDictPatch
]

{ #category : 'accessing' }
RwGsSymbolDictionaryRegistry class >> _methodExtensionPatchClass [

	^ RwGsMethodExtensionSymbolDictPatch
]

{ #category : 'accessing' }
RwGsSymbolDictionaryRegistry class >> _methodExtensionSessionMethodsDeletionPatchClass [

	^ RwGsMethodDeletionExtensionSessionMethodSymbolDictPatch
]

{ #category : 'accessing' }
RwGsSymbolDictionaryRegistry class >> _methodExtensionSessionMethodSourcePatchClass [

	^ RwGsMethodExtensionSessionMethodSourceSymbolDictPatch
]

{ #category : 'accessing' }
RwGsSymbolDictionaryRegistry class >> _methodExtensionSessionMethodsPatchClass [

	^ RwGsMethodExtensionSessionMethodSymbolDictPatch
]

{ #category : 'accessing' }
RwGsSymbolDictionaryRegistry class >> _methodPropertiesPatchClass [

	^ RwGsMethodPropertiesSymDictPatch
]

{ #category : 'accessing' }
RwGsSymbolDictionaryRegistry class >> _methodSourcePatchClass [

	^ RwGsMethodSourceSymbolDictPatch
]

{ #category : 'accessing' }
RwGsSymbolDictionaryRegistry class >> _packageAdditionPatchClass [

	^ RwGsPackageAdditionSymbolDictPatch
]

{ #category : 'accessing' }
RwGsSymbolDictionaryRegistry class >> _packageDeletionPatchClass [

	^ RwGsPackageDeletionSymbolDictPatch
]

{ #category : 'accessing' }
RwGsSymbolDictionaryRegistry class >> defaultProtocolString [

	^ 'as yet unclassified'
]

{ #category : 'installation' }
RwGsSymbolDictionaryRegistry class >> globalSymbol [

	^ #'RwSymbolDictionaryRegistry'
]

{ #category : 'installation' }
RwGsSymbolDictionaryRegistry class >> installIn: aSymbolDictionary [

	| registry |
	registry := self newForSymbolDictionary: aSymbolDictionary.
	aSymbolDictionary at: self globalSymbol put: registry.
	^ registry
]

{ #category : 'instance creation' }
RwGsSymbolDictionaryRegistry class >> newForSymbolDictionary: aSymbolDictionary [

	^ self new
		initialize;
		_symbolDictionary: aSymbolDictionary;
		yourself
]

{ #category : 'method - private api' }
RwGsSymbolDictionaryRegistry >> _doDeleteCompiledMethod: compiledMethod from: behavior [

	"delete a compiled method from behavior"

	| selector |
	selector := compiledMethod selector.
	behavior removeSelector: selector asString environmentId: 0
]

{ #category : 'method - private api' }
RwGsSymbolDictionaryRegistry >> _doDeleteCompiledMethodFromLoadedThings: compiledMethod for: behavior [

	"remove a compiled method from loaded things"

	| loadedMethod loadedPackage loadedClassOrExtension |
	loadedMethod := methodRegistry
		at: compiledMethod
		ifAbsent: [ 
			self
				error:
					'Internal error -- no existing LoadedMethod found for deleted method.' ].

	methodRegistry removeKey: compiledMethod.

	loadedPackage := loadedMethod loadedPackage.
	loadedClassOrExtension := loadedPackage
		classOrExtensionForClass: behavior
		ifAbsent: [ 
			self
				error:
					'Internal error -- attempt to remove a method from a package in which its class is neither defined nor extended.' ].

	loadedClassOrExtension removeLoadedMethod: loadedMethod
]

{ #category : 'private' }
RwGsSymbolDictionaryRegistry >> _loadedClassFor: class noNewVersion: noNewVersionBlock [

	self
		_loadedClassFor: class
		noNewVersion: noNewVersionBlock
		newVersion: [ :loadedClass | 
			self
				error:
					'internal error - the value associated with the key ' , class name printString
						, ' is not identical to the given class' ]
]

{ #category : 'private' }
RwGsSymbolDictionaryRegistry >> _loadedClassFor: class noNewVersion: noNewVersionBlock newVersion: newVersionBlock [

	| loadedClass classKey |
	(class isKindOf: Class)
		ifFalse: [ self error: 'internal error - expected a class' ].
	loadedClass := classRegistry
		at: class classHistory
		ifAbsent: [ 
			self
				error:
					'internal error - No package found for the class ' , class name printString ].
	classKey := loadedClass key asSymbol.
	self
		_symbolDictionaryAssociationAt: classKey
		ifPresent: [ :assoc | 
			assoc value == class
				ifTrue: [ ^ noNewVersionBlock cull: loadedClass cull: assoc ]
				ifFalse: [ ^ newVersionBlock cull: loadedClass cull: assoc ] ].

	self
		error:
			'internal error - there is no assocation present in the receiver for the given class '
				, classKey asString printString
]

{ #category : 'private' }
RwGsSymbolDictionaryRegistry >> _symbolDictionary [

	^ symbolDictionary
]

{ #category : 'private' }
RwGsSymbolDictionaryRegistry >> _symbolDictionary: aSymbolDictionary [

	symbolDictionary := aSymbolDictionary
]

{ #category : 'private' }
RwGsSymbolDictionaryRegistry >> _symbolDictionaryAssociationAt: aKey ifPresent: oneArgBlock [

	"Lookup the given key in the receiver's symbolDictionary. If it is present, answer the value of evaluating the oneArgBlock with the symbol association associated with the key, otherwise answer nil."

	| anAssoc |
	anAssoc := self _symbolDictionary associationAt: aKey otherwise: nil.
	anAssoc ifNotNil: [ ^ oneArgBlock cull: anAssoc ].
	^ nil
]

{ #category : 'private' }
RwGsSymbolDictionaryRegistry >> _symbolDictionaryAssociationAt: key ifPresent: oneArgBlock ifAbsent: absentBlock [

	"Lookup the given key in the receiver's symbolDictionary. If it is present, answer the value of evaluating the oneArgBlock with the symbol association associated with the key, otherwise answer the value of absentBlock."

	self
		_symbolDictionaryAssociationAt: key
		ifPresent: [ :v | ^ oneArgBlock cull: v ].
	^ absentBlock value
]

{ #category : 'private' }
RwGsSymbolDictionaryRegistry >> _symbolDictionaryAt: key ifPresent: oneArgBlock [

	"Lookup the given key in the receiver's symbolDictionary. If it is present, answer the value of evaluating the given block with the value associated with the key. Otherwise, answer nil."

	| v |
	v := self _symbolDictionary at: key ifAbsent: [ ^ nil ].
	^ oneArgBlock cull: v
]

{ #category : 'private' }
RwGsSymbolDictionaryRegistry >> _symbolDictionaryAt: key ifPresent: oneArgBlock ifAbsent: absentBlock [

	"Lookup the given key in the receiver's symbolDictionary. If it is present, answer the value of evaluating the oneArgBlock with the value associated with the key, otherwise answer the value of absentBlock."

	| v |
	v := self _symbolDictionary at: key ifAbsent: [ ^absentBlock value ].
	^ oneArgBlock cull: v

]

{ #category : 'private' }
RwGsSymbolDictionaryRegistry >> addClassAssociation: assoc forClass: class toPackageNamed: packageName [

	"Copy the name association to the correct 
        SymbolDictionary in the live SymbolList.
        Create a LoadedClass for the new class, add it to the defining LoadedPackage."

	| loadedPackage loadedClass |
	loadedPackage := self packageRegistry
		at: packageName
		ifAbsent: [ 
			self packageRegistry
				at: packageName
				put: (RwGsLoadedSymbolDictPackage newNamed: packageName) ].

	classRegistry
		at: class classHistory
		ifAbsent: [ 
			loadedClass := RwGsLoadedSymbolDictClass newForClass: class.
			loadedPackage addLoadedClass: loadedClass.
			loadedClass updatePropertiesFromClassFor: self.
			classRegistry at: class classHistory put: loadedClass.
			(loadedPackage loadedClassExtensions at: class name ifAbsent: [  ])
				ifNotNil: [ :loadedClassExtension | 
					"I think we need to subsume the loadedClassExtension methods into a loadedClass ..."
					self halt ] ].

	self _symbolDictionaryAt: assoc key 
		ifPresent: [ 
			(RwExistingAssociationWithSameKeyNotification new 
				errorMessage: 'internal error - there is an existing assocition with the same key: ' , assoc key printString) signal ]
		ifAbsent: [
			assoc value: class.
			self _symbolDictionary add: assoc]

]

{ #category : 'private' }
RwGsSymbolDictionaryRegistry >> addExtensionCompiledMethod: compiledMethod for: behavior protocol: protocolString toPackageNamed: packageName [

	| methodDictionary selector protocolSymbol existing loadedMethod loadedPackage loadedClassExtension |
	methodDictionary := behavior persistentMethodDictForEnv: 0.
	selector := compiledMethod selector.
	methodDictionary at: selector put: compiledMethod.

	protocolSymbol := protocolString asSymbol.
	(behavior includesCategory: protocolSymbol)
		ifFalse: [ behavior addCategory: protocolSymbol ].
	behavior moveMethod: selector toCategory: protocolSymbol.

	existing := methodRegistry at: compiledMethod ifAbsent: [ nil ].
	existing
		ifNotNil: [ 
			self
				error:
					'Internal error -- existing LoadedMethod found for extension compiled method.' ].
	loadedMethod := RwGsLoadedSymbolDictMethod forMethod: compiledMethod.

	methodRegistry at: compiledMethod put: loadedMethod.

	loadedPackage := self packageRegistry
		at: packageName
		ifAbsent: [ 
			self packageRegistry
				at: packageName
				put: (RwGsLoadedSymbolDictPackage newNamed: packageName) ].

	loadedClassExtension := loadedPackage
		loadedClassExtensionForClass: behavior
		ifAbsent: [ 
			| class ext |
			class := behavior theNonMetaClass.
			ext := RwGsLoadedSymbolDictClassExtension
				newForClass: class
				inPackage: loadedPackage.
			(self classExtensionRegistry
				at: class classHistory
				ifAbsentPut: [ IdentitySet new ]) add: ext.
			ext ].
	loadedClassExtension addLoadedMethod: loadedMethod
]

{ #category : 'private' }
RwGsSymbolDictionaryRegistry >> addExtensionSessionMethods: methDict catDict: catDict for: behavior toPackageNamed: packageName [

	"expecting only a single method to be in methDict/catDict"

	| dictsArray mDict cDict existing loadedMethod loadedPackage loadedClassExtension compiledMethod |
	dictsArray := self homeSessionMethods methodAndCategoryDictionaryFor: behavior.
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

	existing := methodRegistry at: compiledMethod ifAbsent: [ nil ].
	existing
		ifNotNil: [ 
			self
				error:
					'Internal error -- existing LoadedMethod found for extension compiled method.' ].
	loadedMethod := RwGsLoadedSymbolDictMethod forMethod: compiledMethod.

	methodRegistry at: compiledMethod put: loadedMethod.

	loadedPackage := self packageRegistry
		at: packageName
		ifAbsent: [ 
			self packageRegistry
				at: packageName
				put: (RwGsLoadedSymbolDictPackage newNamed: packageName) ].

	loadedClassExtension := loadedPackage
		loadedClassExtensionForClass: behavior
		ifAbsent: [ 
			| class ext |
			class := behavior theNonMetaClass.
			ext := RwGsLoadedSymbolDictClassExtension
				newForClass: class
				inPackage: loadedPackage.
			(self classExtensionRegistry
				at: class classHistory
				ifAbsentPut: [ IdentitySet new ]) add: ext.
			ext ].
	loadedClassExtension addLoadedMethod: loadedMethod.

	^ compiledMethod
]

{ #category : 'private' }
RwGsSymbolDictionaryRegistry >> addNewClassVersionToAssociation: newClass [

	"a new class version is being added to the association in the receiver previously occupied by the original class"

	self
		_loadedClassFor: newClass
		noNewVersion: [ 
			"newClass is actually not a new class version of existing class, that's okay... update properties anyway"
			self updateClassProperties: newClass.
			^ self ]
		newVersion: [ :loadedClass :assoc | 
			"association for class is present, install in association and update the loadedthing"
			assoc value: newClass.
			loadedClass handleClassDeletionOrNewVersion
				ifTrue: [ loadedClass updatePropertiesFromClassFor: self ]
				ifFalse: [ self error: 'internal error - new version of class not properly installed' ] ]
]

{ #category : 'private' }
RwGsSymbolDictionaryRegistry >> addNewCompiledMethod: compiledMethod for: behavior protocol: protocolString toPackageNamed: packageName [

	| methodDictionary selector protocolSymbol existing loadedMethod loadedPackage loadedClassOrExtension |
	methodDictionary := behavior persistentMethodDictForEnv: 0.
	selector := compiledMethod selector.
	(methodDictionary at: selector ifAbsent: [  ])
		ifNotNil: [ :oldCompiledMethod | 
			"there is an existing compiled method ... that means we're adding a recompiled methoded and moving it to the (possibly new) protocol"
			^ self
				addRecompiledMethod: compiledMethod;
				moveCompiledMethod: compiledMethod toProtocol: protocolString;
				yourself ].
	methodDictionary at: selector put: compiledMethod.

	protocolSymbol := protocolString asSymbol.
	(behavior includesCategory: protocolSymbol)
		ifFalse: [ behavior addCategory: protocolSymbol ].
	behavior moveMethod: selector toCategory: protocolSymbol.

	existing := methodRegistry at: compiledMethod ifAbsent: [ nil ].
	existing
		ifNotNil: [ self error: 'Internal error -- existing LoadedMethod found for compiled method.' ].
	loadedMethod := RwGsLoadedSymbolDictMethod forMethod: compiledMethod.

	methodRegistry at: compiledMethod put: loadedMethod.

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

{ #category : 'private' }
RwGsSymbolDictionaryRegistry >> addRecompiledMethod: newCompiledMethod [

	"add a recompiled compiled method to behavior and update the loaded things"

	| selector behavior methodDictionary oldCompiledMethod loadedMethod |
	selector := newCompiledMethod selector.
	behavior := newCompiledMethod inClass.
	methodDictionary := behavior persistentMethodDictForEnv: 0.
	oldCompiledMethod := methodDictionary
		at: selector
		ifAbsent: [ 
			self
				error:
					'Internal error -- expected an existing compiled method in the method dictionary' ].

	oldCompiledMethod == newCompiledMethod
		ifTrue: [ 
			"exit early, no more work to be done"
			^ self ].
	methodDictionary at: selector put: newCompiledMethod.

	loadedMethod := methodRegistry
		at: oldCompiledMethod
		ifAbsent: [ 
			self
				error:
					'Internal error -- no existing LoadedMethod found for the old compiledMethod.' ].
	methodRegistry removeKey: oldCompiledMethod.
	loadedMethod handle: newCompiledMethod.
	methodRegistry at: newCompiledMethod put: loadedMethod
]

{ #category : 'private' }
RwGsSymbolDictionaryRegistry >> addRecompiledSessionMethodMethod: newCompiledMethod [

	"add a recompiled session method compiled method to behavior and update the loaded things"

	| selector behavior dictsArray mDict cDict oldCompiledMethod loadedMethod |
	selector := newCompiledMethod selector.
	behavior := newCompiledMethod inClass.
	dictsArray := self homeSessionMethods methodAndCategoryDictionaryFor: behavior.
	mDict := dictsArray at: 1.
	cDict := dictsArray at: 2.
	oldCompiledMethod := mDict
		at: selector
		ifAbsent: [ 
			self
				error:
					'Internal error -- expected an existing compiled method in the session method method dictionary' ].

	oldCompiledMethod == newCompiledMethod
		ifTrue: [ 
			"exit early, no more work to be done"
			^ self ].
	mDict at: selector put: newCompiledMethod.
	GsPackagePolicy current
		updateMethodLookupCacheFor: newCompiledMethod
		in: behavior.

	loadedMethod := methodRegistry
		at: oldCompiledMethod
		ifAbsent: [ 
			self
				error:
					'Internal error -- no existing LoadedMethod found for the old compiledMethod.' ].
	methodRegistry removeKey: oldCompiledMethod.
	loadedMethod handle: newCompiledMethod.
	methodRegistry at: newCompiledMethod put: loadedMethod
]

{ #category : 'accessing' }
RwGsSymbolDictionaryRegistry >> classExtensionRegistry [

	^ classExtensionRegistry
]

{ #category : 'accessing' }
RwGsSymbolDictionaryRegistry >> classRegistry [

   ^classRegistry

]

{ #category : 'package - patch api' }
RwGsSymbolDictionaryRegistry >> createLoadedPackageFromDefinition: packageDefinition [

	^ self
		createLoadedPackageNamed: packageDefinition key
		properties: packageDefinition properties
]

{ #category : 'package - patch api' }
RwGsSymbolDictionaryRegistry >> createLoadedPackageNamed: packageName properties: properties [

	| existingLoadedPackage newLoadedPackage |
	existingLoadedPackage := packageRegistry at: packageName ifAbsent: [ nil ].
	existingLoadedPackage
		ifNotNil: [ self error: 'Internal error -- Attempt to add a package that exists.' ].
	newLoadedPackage := RwGsLoadedSymbolDictPackage newNamed: packageName.
	newLoadedPackage setPropertiesTo: properties.
	packageRegistry at: packageName put: newLoadedPackage.
	^ newLoadedPackage
]

{ #category : 'class - patch api' }
RwGsSymbolDictionaryRegistry >> deleteClassFromPackage: class [

	"a class association is being deleted from the receiver remove it from the loaded things"

	self
		_loadedClassFor: class
		noNewVersion: [ :loadedClass | 
			"association for class is present, finish delete operation"
			loadedClass removeFromLoaded: self.
			classRegistry removeKey: class classHistory.
			self _symbolDictionary removeKey: loadedClass key asSymbol ]
]

{ #category : 'class - patch api' }
RwGsSymbolDictionaryRegistry >> deleteClassNamedFromPackage: className [

	"a class association is being deleted from the receiver remove it from the loaded things"

	| class |
	class := self _symbolDictionary
		at: className asSymbol
		ifAbsent: [ self error: 'No class found for the given class name: ' , className printString ].
	self deleteClassFromPackage: class
]

{ #category : 'method - patch api' }
RwGsSymbolDictionaryRegistry >> deleteCompiledMethod: compiledMethod from: behavior [

	"delete a compiled method from behavior and remove it from the loaded things"

	self _doDeleteCompiledMethod: compiledMethod from: behavior.

	self _doDeleteCompiledMethodFromLoadedThings: compiledMethod for: behavior
]

{ #category : 'method - patch api' }
RwGsSymbolDictionaryRegistry >> deleteMethod: selector for: behavior [

	| compiledMethod |

	compiledMethod := behavior compiledMethodAt: selector.
	self deleteCompiledMethod: compiledMethod from: behavior

]

{ #category : 'method - patch api' }
RwGsSymbolDictionaryRegistry >> deletePackage: packageName [

	| loadedPackage |
	loadedPackage := packageRegistry
		removeKey: packageName
		ifAbsent: [ self error: 'package ' , packageName printString , ' not found' ].
	loadedPackage loadedProject removeLoadedPackage: loadedPackage
]

{ #category : 'class - patch api' }
RwGsSymbolDictionaryRegistry >> ensureExtensionClassNamed: className existsForPackageNamed: packageName [

	| extensionClass loadedPackage loadedClassExtension |
	extensionClass := self _symbolDictionary
		at: className asSymbol
		ifAbsent: [ 
			self
				error:
					'The extension class named ' , className printString
						, ' cannot be foud in this symbol dictionary (' , self name printString
						,
							'). Packages that extend a class must be loaded into the symbol dictionary in which the class is installed.' ].
	loadedPackage := self packageRegistry
		at: packageName
		ifAbsent: [ 
			self packageRegistry
				at: packageName
				put: (RwGsLoadedSymbolDictPackage newNamed: packageName) ].

	loadedPackage
		loadedClassExtensionForClass: extensionClass
		ifAbsent: [ 
			loadedClassExtension := RwGsLoadedSymbolDictClassExtension
				newForClass: extensionClass
				inPackage: loadedPackage.
			(self classExtensionRegistry
				at: extensionClass classHistory
				ifAbsentPut: [ IdentitySet new ]) add: loadedClassExtension ]
]

{ #category : 'loaded queries' }
RwGsSymbolDictionaryRegistry >> existingForClass: aClass [

	"Answer the registered LoadedClass for the given class."

	^ self
		existingForClass: aClass
		ifAbsent: [ self error: 'no loaded class found for ' , aClass name printString ]
]

{ #category : 'loaded queries' }
RwGsSymbolDictionaryRegistry >> existingForClass: aClass ifAbsent: absentBlock [

	"Answer the registered LoadedClass for the given class."

	^ classRegistry at: aClass classHistory ifAbsent: absentBlock
]

{ #category : 'session methods' }
RwGsSymbolDictionaryRegistry >> homeSessionMethods [

	| package |
	package := self _symbolDictionary at: GsPackage globalName otherwise: nil.
	package ifNil: [ package := GsPackage installIn: self _symbolDictionary ].
	^ package
]

{ #category : 'initialization' }
RwGsSymbolDictionaryRegistry >> initialize [

	packageRegistry := KeyValueDictionary new.	"keyed by package name"
	classRegistry := IdentityKeyValueDictionary new.	"keyed by class classHistory"
	classExtensionRegistry := IdentityKeyValueDictionary new.	"keyed by class classHistory"
	methodRegistry := IdentityKeyValueDictionary new	"keyed by compiledMethod"
]

{ #category : 'loaded queries' }
RwGsSymbolDictionaryRegistry >> loadedHybridPackageNamed: hybridPackageName ifAbsent: absentBlock [

	| packageName |
	(hybridPackageName at: 1) = $*
		ifFalse: [ 
			self
				error:
					'Improperly formed hybrid package name' , hybridPackageName printString
						, '. Expected a leading ''*''' ].
	packageName := (hybridPackageName copyFrom: 2 to: hybridPackageName size)
		asLowercase.
	packageRegistry keys
		do: [ :key | 
			key asLowercase = packageName
				ifTrue: [ ^ packageRegistry at: key ] ].
	^ absentBlock value
]

{ #category : 'loaded queries' }
RwGsSymbolDictionaryRegistry >> loadedPackageDefinitionNamed: packageName ifAbsent: absentBlock [

	^ (self loadedPackageNamed: packageName ifAbsent: absentBlock) asDefinition
]

{ #category : 'loaded queries' }
RwGsSymbolDictionaryRegistry >> loadedPackageDefinitionsNamed: someNames [

	"Answer a PackageSetDefinition containing packages with the given names in the image.
 	If any name is not a loaded package, that name is ignored."

	^ (self loadedPackagesNamed: someNames) asDefinition
]

{ #category : 'loaded queries' }
RwGsSymbolDictionaryRegistry >> loadedPackageNamed: packageName ifAbsent: absentBlock [

	^ packageRegistry at: packageName ifAbsent: absentBlock
]

{ #category : 'loaded queries' }
RwGsSymbolDictionaryRegistry >> loadedPackagesNamed: someNames [

	"Answer a PackageSet containing packages with the given names in the image.
 	If any name is not a loaded package, that name is ignored."

	"very likely we should be dealing with a project name 
at this point ... unless there is a use case for loading a collection of packages without directly using the project interface --- loaded packages do know what project they belong to ... so there might be something done behind the scenes"

	| loadedPackages |
	loadedPackages := someNames
		collect: [ :aName | self loadedPackageNamed: aName ifAbsent: [ nil ] ].
	self
		halt:
			'are someNames allowed to cross project lines if not , then we should allow this method to be used'.
	^ RwEntitySet withAll: (loadedPackages select: [ :each | each notNil ])
]

{ #category : 'accessing' }
RwGsSymbolDictionaryRegistry >> methodRegistry [

   ^methodRegistry

]

{ #category : 'method - patch api' }
RwGsSymbolDictionaryRegistry >> moveCompiledMethod: compiledMethod toProtocol: newProtocol [

	"move a compiled method into a different protocol and update loaded things"

	| behavior selector loadedMethod oldCat catSym catDict methodDictionary existingCompiledMethod |
	selector := compiledMethod selector.
	behavior := compiledMethod inClass.

	methodDictionary := behavior persistentMethodDictForEnv: 0.
	existingCompiledMethod := methodDictionary
		at: selector
		ifAbsent: [ 
			self
				error:
					'Internal error -- no existing CompileMethod found for patched method.' ].
	existingCompiledMethod == compiledMethod
		ifFalse: [ 
			self
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

	loadedMethod := methodRegistry
		at: compiledMethod
		ifAbsent: [ 
			self
				error:
					'Internal error -- no existing LoadedMethod found for the compiledMethod.' ].

	loadedMethod updateForProtocolChange
]

{ #category : 'accessing' }
RwGsSymbolDictionaryRegistry >> packageRegistry [

   ^packageRegistry

]

{ #category : 'class - patch api' }
RwGsSymbolDictionaryRegistry >> updateClassProperties: class [

	"class properties have changed, update the clas properties of the loaded things"

	(class isKindOf: Class)
		ifFalse: [ self error: 'internal error - expected a class' ].
	self
		_loadedClassFor: class
		noNewVersion: [ :loadedClass | 
			"association for class is present, update the loaded thing"
			loadedClass updatePropertiesFromClassFor: self ]
]
