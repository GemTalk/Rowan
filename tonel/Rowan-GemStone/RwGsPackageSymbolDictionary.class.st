Class {
	#name : 'RwGsPackageSymbolDictionary',
	#superclass : 'SymbolDictionary',
	#instVars : [
		'packageRegistry',
		'classRegistry',
		'methodRegistry'
	],
	#category : 'Rowan-GemStone'
}

{ #category : 'instance creation' }
RwGsPackageSymbolDictionary class >> newNamed: aSymbol [

	^ self new
		name: aSymbol;
		yourself
]

{ #category : 'private' }
RwGsPackageSymbolDictionary >> _initializeWithoutClear: newSize [

	super _initializeWithoutClear: newSize.
	packageRegistry := KeyValueDictionary new.
	classRegistry := IdentityKeyValueDictionary new.	"keyed by class classHistory"
	methodRegistry := IdentityKeyValueDictionary new.	"keyed by compiledMethod"
	^ self
]

{ #category : 'private' }
RwGsPackageSymbolDictionary >> _loadedClassFor: class noNewVersion: noNewVersionBlock [

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
RwGsPackageSymbolDictionary >> _loadedClassFor: class noNewVersion: noNewVersionBlock newVersion: newVersionBlock [

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
		associationAt: classKey
		ifPresent: [ :assoc | 
			assoc value == class
				ifTrue: [ ^ noNewVersionBlock cull: loadedClass cull: assoc ]
				ifFalse: [ ^ newVersionBlock cull: loadedClass cull: assoc ] ].

	self
		error:
			'internal error - there is no assocation present in the receiver for the given class '
				, classKey asString printString
]

{ #category : 'class' }
RwGsPackageSymbolDictionary >> addClassAssociation: assoc toPackageNamed: packageName [

	"a class association is being added to the named package in the receiver"

	| loadedPackage class loadedClass |
	class := assoc value.
	(class isKindOf: Class)
		ifFalse: [ self error: 'internal error - expected a class association' ].
	loadedPackage := self packageRegistry
		at: packageName
		ifAbsent: [ 
			self packageRegistry
				at: packageName
				put: (RwGSLoadedSymbolDictPackage newNamed: packageName) ].
	self
		at: assoc key
		ifPresent: [ 
			self
				error:
					'internal error - there is an existing assocition with the same key: '
						, assoc key printString ].
	classRegistry
		at: class classHistory
		ifAbsent: [ 
			loadedClass := RwGsLoadedSymbolDictClass newForClass: class.
			loadedPackage addLoadedClass: loadedClass.
			loadedClass updatePropertiesFromClassFor: self.
			classRegistry at: class classHistory put: loadedClass.
			loadedPackage loadedClassExtensions
				at: class name
				ifPresent: [ :loadedClassExtension | 
					"I think we need to subsume the loadedClassExtension methods into a loadedClass ..."
					self halt ] ].
	self add: assoc
]

{ #category : 'class' }
RwGsPackageSymbolDictionary >> addNewClassVersionToAssociation: newClass [

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

{ #category : 'method' }
RwGsPackageSymbolDictionary >> addNewCompiledMethod: compiledMethod into: behavior protocol: protocolString toPackageNamed: packageName [

	| methodDictionary selector protocolSymbol existing loadedMethod loadedPackage loadedClassOrExtension properties |
	methodDictionary := behavior persistentMethodDictForEnv: 0.
	selector := compiledMethod selector.
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

	loadedPackage := self packageRegistry
		at: packageName
		ifAbsent: [ 
			self
				error: 'Internal error -- attempt to add a method to a nonexistent package.' ].

	loadedClassOrExtension := loadedPackage
		classOrExtensionForClassNamed: behavior theNonMetaClass name asString
		ifAbsent: [ 
			self
				error:
					'Internal error -- attempt to add a method to a package in which its class is neither defined nor extended.' ].
	loadedClassOrExtension addLoadedMethod: loadedMethod.

]

{ #category : 'method' }
RwGsPackageSymbolDictionary >> addRecompiledMethod: newCompiledMethod [

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
					'Internal error -- no existing CompileMethod found for patched method: '
						, selector printString ].

	oldCompiledMethod == newCompiledMethod
		ifTrue: [ 
			self
				error:
					'Internal error -- The new recompiled method is identical to the installed method '
						, selector printString ].
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

{ #category : 'accessing' }
RwGsPackageSymbolDictionary >> associationAt: aKey ifPresent: oneArgBlock [

	"Lookup the given key in the receiver. If it is present, answer the value of evaluating the oneArgBlock with the symbol association associated with the key, otherwise answer nil."

	| anAssoc |
	anAssoc := self associationAt: aKey otherwise: nil.
	anAssoc ifNotNil: [ ^ oneArgBlock value: anAssoc ].
	^ nil
]

{ #category : 'accessing' }
RwGsPackageSymbolDictionary >> associationAt: key ifPresent: oneArgBlock ifAbsent: absentBlock [

	"Lookup the given key in the receiver. If it is present, answer the value of evaluating the oneArgBlock with the symbol association associated with the key, otherwise answer the value of absentBlock."

	self associationAt: key ifPresent: [ :v | ^ oneArgBlock value: v ].
	^ absentBlock value
]

{ #category : 'accessing' }
RwGsPackageSymbolDictionary >> at: key ifPresent: aBlock [

	"Lookup the given key in the receiver. If it is present, answer the value of evaluating the given block with the value associated with the key. Otherwise, answer nil."

	| v |
	v := self at: key ifAbsent: [ ^ nil ].
	^ aBlock value: v
]

{ #category : 'accessing' }
RwGsPackageSymbolDictionary >> at: key ifPresent: oneArgBlock ifAbsent: absentBlock [

	"Lookup the given key in the receiver. If it is present, answer the value of evaluating the oneArgBlock with the value associated with the key, otherwise answer the value of absentBlock."

	self at: key ifPresent: [ :v | ^ oneArgBlock value: v ].
	^ absentBlock value
]

{ #category : 'private - migration avoidance' }
RwGsPackageSymbolDictionary >> become: ignored [

	"skip migration until the class definition settles down a bit"

	
]

{ #category : 'accessing' }
RwGsPackageSymbolDictionary >> classRegistry [

   ^classRegistry

]

{ #category : 'class' }
RwGsPackageSymbolDictionary >> deleteClassFromPackage: class [

	"a class association is being deleted from the receiver remove it from the loaded things"

	self
		_loadedClassFor: class
		noNewVersion: [ :loadedClass | 
			"association for class is present, finish delete operation"
			loadedClass removeFromLoaded.
			classRegistry removeKey: class classHistory.
			self removeKey: loadedClass key asSymbol ]
]

{ #category : 'method' }
RwGsPackageSymbolDictionary >> deleteCompiledMethod: compiledMethod from: behavior [

	"delete a compiled method from behavior and remove it from the loaded things"

	| selector loadedMethod loadedPackage loadedClassOrExtension |
	selector := compiledMethod selector.
	behavior removeSelector: selector asString environmentId: 0.

	loadedMethod := methodRegistry
		at: compiledMethod
		ifAbsent: [ 
			self
				error:
					'Internal error -- no existing LoadedMethod found for deleted method.' ].

	methodRegistry removeKey: compiledMethod.

	loadedPackage := loadedMethod loadedPackage.
	loadedClassOrExtension := loadedPackage
		classOrExtensionForClassNamed: behavior theNonMetaClass name asString
		ifAbsent: [ 
			self
				error:
					'Internal error -- attempt to add a method to a package in which its class is neither defined nor extended.' ].

	loadedClassOrExtension removeLoadedMethod: loadedMethod
]

{ #category : 'accessing' }
RwGsPackageSymbolDictionary >> methodRegistry [

   ^methodRegistry

]

{ #category : 'private - migration avoidance' }
RwGsPackageSymbolDictionary >> migrateFrom: anotherObject instVarMap: otherivi [

	"skip migration until the class definition settles down a bit"

	
]

{ #category : 'private - migration avoidance' }
RwGsPackageSymbolDictionary >> migrateInstances: instances to: anotherClass [

	"skip migration until the class definition settles down a bit"

	
]

{ #category : 'method' }
RwGsPackageSymbolDictionary >> moveCompiledMethod: compiledMethod toProtocol: newProtocol [

	"delete a compiled method from behavior and remove it from the loaded things"

	| behavior selector loadedMethod oldCat catSym catDict |
	selector := compiledMethod selector.
	behavior := compiledMethod inClass.

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
RwGsPackageSymbolDictionary >> packageRegistry [

   ^packageRegistry

]

{ #category : 'class' }
RwGsPackageSymbolDictionary >> updateClassProperties: class [

	"class properties have changed, update the clas properties of the loaded things"

	(class isKindOf: Class)
		ifFalse: [ self error: 'internal error - expected a class' ].
	self
		_loadedClassFor: class
		noNewVersion: [ :loadedClass | 
			"association for class is present, update the loaded thing"
			loadedClass updatePropertiesFromClassFor: self ]
]
