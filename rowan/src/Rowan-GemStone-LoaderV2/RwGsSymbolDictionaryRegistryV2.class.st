Class {
	#name : 'RwGsSymbolDictionaryRegistryV2',
	#superclass : 'Object',
	#instVars : [
		'symbolDictionary',
		'packageRegistry',
		'classRegistry',
		'classExtensionRegistry',
		'methodRegistry'
	],
	#category : 'Rowan-GemStone-LoaderV2'
}

{ #category : 'accessing' }
RwGsSymbolDictionaryRegistryV2 class >> registry_ImplementationClass [

	^ RwGsSymbolDictionaryRegistry_ImplementationV2

]

{ #category : 'accessing' }
RwGsSymbolDictionaryRegistryV2 class >> defaultProtocolString [

	^ 'as yet unclassified'
]

{ #category : 'installation' }
RwGsSymbolDictionaryRegistryV2 class >> globalSymbol [

	^ #'RwSymbolDictionaryRegistry'
]

{ #category : 'installation' }
RwGsSymbolDictionaryRegistryV2 class >> installIn: aSymbolDictionary [

	| registry |
	registry := self newForSymbolDictionary: aSymbolDictionary.
	aSymbolDictionary at: self globalSymbol put: registry.
	^ registry
]

{ #category : 'instance creation' }
RwGsSymbolDictionaryRegistryV2 class >> newForSymbolDictionary: aSymbolDictionary [

	^ self new
		_symbolDictionary: aSymbolDictionary;
		initialize;
		yourself

]

{ #category : 'method - private api' }
RwGsSymbolDictionaryRegistryV2 >> _doDeleteCompiledMethodFromLoadedThings: compiledMethod for: behavior [

	"remove a compiled method from loaded things"

	^ self class registry_ImplementationClass _doDeleteCompiledMethodFromLoadedThings: compiledMethod for: behavior instance: self

]

{ #category : 'method - private api' }
RwGsSymbolDictionaryRegistryV2 >> _doDeleteCompiledMethodFromLoadedThings: compiledMethod for: behavior implementationClass: implementationClass [

	"remove a compiled method from loaded things"

	"Use for calls from classes in Rowan-GemStone-Loader package"

	^ implementationClass _doDeleteCompiledMethodFromLoadedThings: compiledMethod for: behavior instance: self

]

{ #category : 'private' }
RwGsSymbolDictionaryRegistryV2 >> _loadedClassFor: class noNewVersion: noNewVersionBlock [

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
RwGsSymbolDictionaryRegistryV2 >> _loadedClassFor: class noNewVersion: noNewVersionBlock newVersion: newVersionBlock [

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
RwGsSymbolDictionaryRegistryV2 >> _symbolDictionary [

	^ symbolDictionary
]

{ #category : 'private' }
RwGsSymbolDictionaryRegistryV2 >> _symbolDictionary: aSymbolDictionary [

	symbolDictionary := aSymbolDictionary
]

{ #category : 'private' }
RwGsSymbolDictionaryRegistryV2 >> _symbolDictionaryAssociationAt: aKey ifPresent: oneArgBlock [

	"Lookup the given key in the receiver's symbolDictionary. If it is present, answer the value of evaluating the oneArgBlock with the symbol association associated with the key, otherwise answer nil."

	| anAssoc |
	anAssoc := self _symbolDictionary associationAt: aKey otherwise: nil.
	anAssoc ifNotNil: [ ^ oneArgBlock cull: anAssoc ].
	^ nil
]

{ #category : 'private' }
RwGsSymbolDictionaryRegistryV2 >> _symbolDictionaryAssociationAt: key ifPresent: oneArgBlock ifAbsent: absentBlock [

	"Lookup the given key in the receiver's symbolDictionary. If it is present, answer the value of evaluating the oneArgBlock with the symbol association associated with the key, otherwise answer the value of absentBlock."

	self
		_symbolDictionaryAssociationAt: key
		ifPresent: [ :v | ^ oneArgBlock cull: v ].
	^ absentBlock value
]

{ #category : 'private' }
RwGsSymbolDictionaryRegistryV2 >> _symbolDictionaryAt: key ifPresent: oneArgBlock [

	"Lookup the given key in the receiver's symbolDictionary. If it is present, answer the value of evaluating the given block with the value associated with the key. Otherwise, answer nil."

	| v |
	v := self _symbolDictionary at: key ifAbsent: [ ^ nil ].
	^ oneArgBlock cull: v
]

{ #category : 'private' }
RwGsSymbolDictionaryRegistryV2 >> _symbolDictionaryAt: key ifPresent: oneArgBlock ifAbsent: absentBlock [

	"Lookup the given key in the receiver's symbolDictionary. If it is present, answer the value of evaluating the oneArgBlock with the value associated with the key, otherwise answer the value of absentBlock."

	| v |
	v := self _symbolDictionary at: key ifAbsent: [ ^absentBlock value ].
	^ oneArgBlock cull: v

]

{ #category : 'private' }
RwGsSymbolDictionaryRegistryV2 >> addClassAssociation: assoc forClass: class toPackageNamed: packageName [

	"Copy the name association to the correct 
        SymbolDictionary in the live SymbolList.
        Create a LoadedClass for the new class, add it to the defining LoadedPackage."

	^ self class registry_ImplementationClass addClassAssociation: assoc forClass: class toPackageNamed: packageName instance: self

]

{ #category : 'private' }
RwGsSymbolDictionaryRegistryV2 >> addClassAssociation: assoc forClass: class toPackageNamed: packageName implementationClass: implementationClass [

	"Copy the name association to the correct 
        SymbolDictionary in the live SymbolList.
        Create a LoadedClass for the new class, add it to the defining LoadedPackage."

	"Use for calls from classes in Rowan-GemStone-Loader package"

	^ implementationClass addClassAssociation: assoc forClass: class toPackageNamed: packageName instance: self

]

{ #category : 'private' }
RwGsSymbolDictionaryRegistryV2 >> addExtensionCompiledMethod: compiledMethod for: behavior protocol: protocolString toPackageNamed: packageName [

	^ self class registry_ImplementationClass addExtensionCompiledMethod: compiledMethod for: behavior protocol: protocolString toPackageNamed: packageName instance: self

]

{ #category : 'private' }
RwGsSymbolDictionaryRegistryV2 >> addExtensionCompiledMethod: compiledMethod for: behavior protocol: protocolString toPackageNamed: packageName implementationClass: implementationClass [

	"Use for calls from classes in Rowan-GemStone-Loader package"

	^ implementationClass addExtensionCompiledMethod: compiledMethod for: behavior protocol: protocolString toPackageNamed: packageName instance: self

]

{ #category : 'private' }
RwGsSymbolDictionaryRegistryV2 >> addExtensionSessionMethods: methDict catDict: catDict for: behavior toPackageNamed: packageName [

	"expecting only a single method to be in methDict/catDict"

	^ self class registry_ImplementationClass addExtensionSessionMethods: methDict catDict: catDict for: behavior toPackageNamed: packageName instance: self

]

{ #category : 'private' }
RwGsSymbolDictionaryRegistryV2 >> addExtensionSessionMethods: methDict catDict: catDict for: behavior toPackageNamed: packageName implementationClass: implementationClass [

	"expecting only a single method to be in methDict/catDict"

	"Use for calls from classes in Rowan-GemStone-Loader package"

	^ implementationClass addExtensionSessionMethods: methDict catDict: catDict for: behavior toPackageNamed: packageName instance: self

]

{ #category : 'private' }
RwGsSymbolDictionaryRegistryV2 >> addMovedDeletedMethod: compiledMethod for: behavior protocol: protocolString toPackageNamed: packageName [

	"there is an existing compiled method that has already been deleted from another package ... so we're adding it
		back using specialized processing"

	^ self class registry_ImplementationClass addMovedDeletedMethod: compiledMethod for: behavior protocol: protocolString toPackageNamed: packageName instance: self

]

{ #category : 'private' }
RwGsSymbolDictionaryRegistryV2 >> addMovedDeletedMethod: compiledMethod for: behavior protocol: protocolString toPackageNamed: packageName implementationClass: implementationClass [

	"there is an existing compiled method that has already been deleted from another package ... so we're adding it
		back using specialized processing"

	"Use for calls from classes in Rowan-GemStone-Loader package"

	^ implementationClass addMovedDeletedMethod: compiledMethod for: behavior protocol: protocolString toPackageNamed: packageName instance: self

]

{ #category : 'private' }
RwGsSymbolDictionaryRegistryV2 >> addNewClassVersionToAssociation: newClass [

	"a new class version is being added to the association in the receiver previously occupied by the original class"

	self class registry_ImplementationClass addNewClassVersionToAssociation: newClass instance: self
]

{ #category : 'private' }
RwGsSymbolDictionaryRegistryV2 >> addNewClassVersionToAssociation: newClass implementationClass: implementationClass [

	"a new class version is being added to the association in the receiver previously occupied by the original class"

	"Use for calls from classes in Rowan-GemStone-Loader package"

	implementationClass addNewClassVersionToAssociation: newClass instance: self
]

{ #category : 'private' }
RwGsSymbolDictionaryRegistryV2 >> addNewCompiledMethod: compiledMethod for: behavior protocol: protocolString toPackageNamed: packageName [

	^ self class registry_ImplementationClass addNewCompiledMethod: compiledMethod for: behavior protocol: protocolString toPackageNamed: packageName instance: self

]

{ #category : 'private' }
RwGsSymbolDictionaryRegistryV2 >> addNewCompiledMethod: compiledMethod for: behavior protocol: protocolString toPackageNamed: packageName implementationClass: implementationClass [

	"Use for calls from classes in Rowan-GemStone-Loader package"

	^ implementationClass addNewCompiledMethod: compiledMethod for: behavior protocol: protocolString toPackageNamed: packageName instance: self

]

{ #category : 'private' }
RwGsSymbolDictionaryRegistryV2 >> addRecompiledMethod: newCompiledMethod [

	"add a recompiled compiled method to behavior and update the loaded things"

	^ self class registry_ImplementationClass addRecompiledMethod: newCompiledMethod instance: self

]

{ #category : 'private' }
RwGsSymbolDictionaryRegistryV2 >> addRecompiledMethod: newCompiledMethod implementationClass: implementationClass [

	"add a recompiled compiled method to behavior and update the loaded things"

	"Use for calls from classes in Rowan-GemStone-Loader package"

	^ implementationClass addRecompiledMethod: newCompiledMethod instance: self

]

{ #category : 'private' }
RwGsSymbolDictionaryRegistryV2 >> addRecompiledSessionMethodMethod: newCompiledMethod [

	"add a recompiled session method compiled method to behavior and update the loaded things"

	^ self class registry_ImplementationClass addRecompiledSessionMethodMethod: newCompiledMethod instance: self

]

{ #category : 'private' }
RwGsSymbolDictionaryRegistryV2 >> addRecompiledSessionMethodMethod: newCompiledMethod implementationClass: implementationClass [

	"add a recompiled session method compiled method to behavior and update the loaded things"

	"Use for calls from classes in Rowan-GemStone-Loader package"

	^ implementationClass addRecompiledSessionMethodMethod: newCompiledMethod instance: self

]

{ #category : 'method - adopt api' }
RwGsSymbolDictionaryRegistryV2 >> adoptCompiledMethod: compiledMethod classExtension: classExtension for: behavior protocol: protocolString toPackageNamed: packageName [

	| methodDictionary selector protocolSymbol existing loadedMethod loadedPackage loadedClassOrExtension |
	classExtension
		ifTrue: [
			"to adopt and extension method, it is much less complicated" 
			^ self 
				addExtensionCompiledMethod: compiledMethod 
				for: behavior 
				protocol: protocolString 
				toPackageNamed: packageName ].

	methodDictionary := (behavior persistentMethodDictForEnv: 0 ) ifNil:[ Dictionary new ].
	selector := compiledMethod selector.
	compiledMethod == (methodDictionary at: selector ifAbsent: [ self error: 'expected an existing compiled method' ])
		ifFalse: [ self error: 'The given compiled method does not the existing compiled method in the class.' ].

	protocolSymbol := protocolString asSymbol.
	(behavior includesCategory: protocolSymbol)
		ifFalse: [ behavior addCategory: protocolSymbol ].
	behavior moveMethod: selector toCategory: protocolSymbol.

	existing := methodRegistry at: compiledMethod ifAbsent: [ nil ].
	existing
		ifNotNil: [
			"existing LoadedMethod found for compiled method ... ignore already packaged methods"
			^ self ].
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

{ #category : 'accessing' }
RwGsSymbolDictionaryRegistryV2 >> classExtensionRegistry [

	^ classExtensionRegistry
]

{ #category : 'accessing' }
RwGsSymbolDictionaryRegistryV2 >> classRegistry [

   ^classRegistry

]

{ #category : 'package - patch api' }
RwGsSymbolDictionaryRegistryV2 >> createLoadedPackageFromDefinition: packageDefinition [

	^ self class registry_ImplementationClass createLoadedPackageFromDefinition: packageDefinition instance: self

]

{ #category : 'package - patch api' }
RwGsSymbolDictionaryRegistryV2 >> createLoadedPackageFromDefinition: packageDefinition implementationClass: implementationClass [

	"Use for calls from classes in Rowan-GemStone-Loader package"

	^ implementationClass createLoadedPackageFromDefinition: packageDefinition instance: self

]

{ #category : 'package - patch api' }
RwGsSymbolDictionaryRegistryV2 >> createLoadedPackageNamed: packageName properties: properties [

	^ self class registry_ImplementationClass createLoadedPackageNamed: packageName properties: properties instance: self

]

{ #category : 'package - patch api' }
RwGsSymbolDictionaryRegistryV2 >> createLoadedPackageNamed: packageName properties: properties  implementationClass: implementationClass [

	"Use for calls from classes in Rowan-GemStone-Loader package"

	^ implementationClass createLoadedPackageNamed: packageName properties: properties instance: self

]

{ #category : 'class - patch api' }
RwGsSymbolDictionaryRegistryV2 >> deleteClassFromPackage: class [

	"a class association is being deleted from the receiver remove it from the loaded things"

	^ self class registry_ImplementationClass deleteClassFromPackage: class instance: self

]

{ #category : 'class - patch api' }
RwGsSymbolDictionaryRegistryV2 >> deleteClassFromPackage: class implementationClass: implementationClass [

	"a class association is being deleted from the receiver remove it from the loaded things"

	"Use for calls from classes in Rowan-GemStone-Loader package"

	^ implementationClass deleteClassFromPackage: class instance: self

]

{ #category : 'class - patch api' }
RwGsSymbolDictionaryRegistryV2 >> deleteClassNamedFromPackage: className [

	"a class association is being deleted from the receiver remove it from the loaded things"

	^ self class registry_ImplementationClass deleteClassNamedFromPackage: className instance: self

]

{ #category : 'class - patch api' }
RwGsSymbolDictionaryRegistryV2 >> deleteClassNamedFromPackage: className implementationClass: implementationClass [

	"a class association is being deleted from the receiver remove it from the loaded things"

	"Use for calls from classes in Rowan-GemStone-Loader package"

	^ implementationClass deleteClassNamedFromPackage: className instance: self

]

{ #category : 'method - patch api' }
RwGsSymbolDictionaryRegistryV2 >> deleteCompiledMethod: compiledMethod from: behavior [

	"delete a compiled method from behavior and remove it from the loaded things"

	^ self class registry_ImplementationClass deleteCompiledMethod: compiledMethod from: behavior instance: self

]

{ #category : 'method - patch api' }
RwGsSymbolDictionaryRegistryV2 >> deleteCompiledMethod: compiledMethod from: behavior implementationClass: implementationClass [

	"delete a compiled method from behavior and remove it from the loaded things"

	"Use for calls from classes in Rowan-GemStone-Loader package"

	^ implementationClass deleteCompiledMethod: compiledMethod from: behavior instance: self

]

{ #category : 'method - patch api' }
RwGsSymbolDictionaryRegistryV2 >> deleteMethod: selector for: behavior [

	^ self class registry_ImplementationClass deleteMethod: selector for: behavior instance: self

]

{ #category : 'method - patch api' }
RwGsSymbolDictionaryRegistryV2 >> deleteMethod: selector for: behavior implementationClass: implementationClass [

	"Use for calls from classes in Rowan-GemStone-Loader package"

	^ implementationClass deleteMethod: selector for: behavior instance: self

]

{ #category : 'method - patch api' }
RwGsSymbolDictionaryRegistryV2 >> deleteMovedLoadedMethod: selector for: behavior implementationClass: implementationClass [

	"Use for calls from classes in Rowan-GemStone-Loader package"

	^ implementationClass deleteMovedLoadedMethod: selector for: behavior instance: self

]

{ #category : 'method - patch api' }
RwGsSymbolDictionaryRegistryV2 >> deleteMovedMethod: selector for: behavior implementationClass: implementationClass [

	"Use for calls from classes in Rowan-GemStone-Loader package"

	^ implementationClass deleteMovedMethod: selector for: behavior instance: self

]

{ #category : 'method - patch api' }
RwGsSymbolDictionaryRegistryV2 >> deletePackage: packageName [

	^  self class registry_ImplementationClass deletePackage: packageName instance: self

]

{ #category : 'class disown api' }
RwGsSymbolDictionaryRegistryV2 >> disownClass: class [

	"Remove the specified class and all of it's packaged instance and class methods from the loaded things."

	^ self class registry_ImplementationClass disownClass: class instance: self

]

{ #category : 'class disown api' }
RwGsSymbolDictionaryRegistryV2 >> disownClassExtensions: class forLoadedPackage: loadedPackage [

	"Remove the extension methods in the named class for the named package"

	^ self class registry_ImplementationClass disownClassExtensions: class forLoadedPackage: loadedPackage instance: self

]

{ #category : 'class - patch api' }
RwGsSymbolDictionaryRegistryV2 >> ensureExtensionClassNamed: className existsForPackageNamed: packageName implementationClass: implementationClass [

	"Use for calls from classes in Rowan-GemStone-Loader package"

	^ implementationClass ensureExtensionClassNamed: className existsForPackageNamed: packageName instance: self

]

{ #category : 'loaded queries' }
RwGsSymbolDictionaryRegistryV2 >> existingForClass: aClass [

	"Answer the registered LoadedClass for the given class."

	^ self
		existingForClass: aClass
		ifAbsent: [ self error: 'no loaded class found for ' , aClass name printString ]
]

{ #category : 'loaded queries' }
RwGsSymbolDictionaryRegistryV2 >> existingForClass: aClass ifAbsent: absentBlock [

	"Answer the registered LoadedClass for the given class."

	^ classRegistry at: aClass classHistory ifAbsent: absentBlock
]

{ #category : 'package - creation api' }
RwGsSymbolDictionaryRegistryV2 >> existingOrNewLoadedPackageNamed: packageName [

	^ self class registry_ImplementationClass existingOrNewLoadedPackageNamed: packageName instance: self

]

{ #category : 'package - creation api' }
RwGsSymbolDictionaryRegistryV2 >> existingOrNewLoadedPackageNamed: packageName implementationClass: implementationClass [

	^ implementationClass existingOrNewLoadedPackageNamed: packageName instance: self

]

{ #category : 'session methods' }
RwGsSymbolDictionaryRegistryV2 >> homeSessionMethods [

	| package |
	package := self _symbolDictionary at: GsPackage globalName otherwise: nil.
	package ifNil: [ package := GsPackage installIn: self _symbolDictionary ].
	^ package
]

{ #category : 'initialization' }
RwGsSymbolDictionaryRegistryV2 >> initialize [

	| symbolDictObjectSecurityPolicy |
	packageRegistry := KeyValueDictionary new.	"keyed by package name"
	classRegistry := IdentityKeyValueDictionary new.	"keyed by class classHistory"
	classExtensionRegistry := IdentityKeyValueDictionary new.	"keyed by class classHistory"
	methodRegistry := IdentityKeyValueDictionary new.	"keyed by compiledMethod"

	symbolDictObjectSecurityPolicy := symbolDictionary objectSecurityPolicy.
	packageRegistry objectSecurityPolicy: symbolDictObjectSecurityPolicy.
	classRegistry objectSecurityPolicy: symbolDictObjectSecurityPolicy.
	classExtensionRegistry objectSecurityPolicy: symbolDictObjectSecurityPolicy.
	methodRegistry objectSecurityPolicy: symbolDictObjectSecurityPolicy.

]

{ #category : 'loaded queries' }
RwGsSymbolDictionaryRegistryV2 >> loadedHybridPackageNamed: hybridPackageName ifAbsent: absentBlock [

	^ self class registry_ImplementationClass loadedHybridPackageNamed: hybridPackageName ifAbsent: absentBlock instance: self

]

{ #category : 'loaded queries' }
RwGsSymbolDictionaryRegistryV2 >> loadedHybridPackageNamed: hybridPackageName ifAbsent: absentBlock implementationClass: implementationClass [

	"Use for calls from classes in Rowan-GemStone-Loader package"

	^ implementationClass loadedHybridPackageNamed: hybridPackageName ifAbsent: absentBlock instance: self

]

{ #category : 'loaded queries' }
RwGsSymbolDictionaryRegistryV2 >> loadedPackageNamed: packageName ifAbsent: absentBlock [

	^ self class registry_ImplementationClass loadedPackageNamed: packageName ifAbsent: absentBlock instance: self

]

{ #category : 'loaded queries' }
RwGsSymbolDictionaryRegistryV2 >> loadedPackageNamed: packageName ifAbsent: absentBlock implementationClass: implementationClass [

	"Use for calls from classes in Rowan-GemStone-Loader package"

	^ implementationClass loadedPackageNamed: packageName ifAbsent: absentBlock instance: self

]

{ #category : 'accessing' }
RwGsSymbolDictionaryRegistryV2 >> methodRegistry [

   ^methodRegistry

]

{ #category : 'method - patch api' }
RwGsSymbolDictionaryRegistryV2 >> moveCompiledMethod: compiledMethod toProtocol: newProtocol [

	"move a compiled method into a different protocol and update loaded things"

	^ self class registry_ImplementationClass moveCompiledMethod: compiledMethod toProtocol: newProtocol instance: self

]

{ #category : 'method - patch api' }
RwGsSymbolDictionaryRegistryV2 >> moveCompiledMethod: compiledMethod toProtocol: newProtocol implementationClass: implementationClass [

	"move a compiled method into a different protocol and update loaded things"

	"Use for calls from classes in Rowan-GemStone-Loader package"

	^ implementationClass moveCompiledMethod: compiledMethod toProtocol: newProtocol instance: self

]

{ #category : 'method - patch api' }
RwGsSymbolDictionaryRegistryV2 >> movePackage: packageName to: symbolDictionaryName classesWithNewVersions: classesWithNewVersions [
	^ self class registry_ImplementationClass
		movePackage: packageName
		to: symbolDictionaryName
		classesWithNewVersions: classesWithNewVersions
		instance: self
]

{ #category : 'package - creation api' }
RwGsSymbolDictionaryRegistryV2 >> newLoadedPackageNamed: packageName [

	^ self class registry_ImplementationClass newLoadedPackageNamed: packageName instance: self

]

{ #category : 'package - creation api' }
RwGsSymbolDictionaryRegistryV2 >> newLoadedPackageNamed: packageName implementationClass: implementationClass [

	^ implementationClass newLoadedPackageNamed: packageName instance: self

]

{ #category : 'accessing' }
RwGsSymbolDictionaryRegistryV2 >> packageRegistry [

   ^packageRegistry

]

{ #category : 'class - patch api' }
RwGsSymbolDictionaryRegistryV2 >> updateClassProperties: class [

	"class properties have changed, update the clas properties of the loaded things"

	^ self class registry_ImplementationClass updateClassProperties: class instance: self

]

{ #category : 'class - patch api' }
RwGsSymbolDictionaryRegistryV2 >> updateClassProperties: class  implementationClass: implementationClass [

	"class properties have changed, update the clas properties of the loaded things"

	"Use for calls from classes in Rowan-GemStone-Loader package"

	^ implementationClass updateClassProperties: class instance: self

]

{ #category : 'class - registration' }
RwGsSymbolDictionaryRegistryV2 >> loadedClassForClass: aClass [
	^ self class registry_ImplementationClass loadedClassForClass: aClass
]

{ #category : 'class - registration' }
RwGsSymbolDictionaryRegistryV2 >> loadedClassForClass: aClass ifAbsent: absentBlock [
	^ self class registry_ImplementationClass loadedClassForClass: aClass ifAbsent: absentBlock
]

{ #category : 'class - registration' }
RwGsSymbolDictionaryRegistryV2 >> registerLoadedClass: loadedClass forClass: aClass [
	^ self class registry_ImplementationClass registerLoadedClass: loadedClass forClass: aClass
]

{ #category : 'class - registration' }
RwGsSymbolDictionaryRegistryV2 >> unregisterLoadedClass: loadedClass forClass: aClass [
	^ self class registry_ImplementationClass unregisterLoadedClass: loadedClass forClass: aClass
]

{ #category : 'class - registration' }
RwGsSymbolDictionaryRegistryV2 >> loadedClassExtensionsForClass: aClass [
	^ self class registry_ImplementationClass loadedClassExtensionsForClass: aClass
]

{ #category : 'class - registration' }
RwGsSymbolDictionaryRegistryV2 >> loadedClassExtensionsForClass: aClass ifAbsent: absentBlock [
	^ self class registry_ImplementationClass loadedClassExtensionsForClass: aClass ifAbsent: absentBlock
]

{ #category : 'class - registration' }
RwGsSymbolDictionaryRegistryV2 >> registerLoadedClassExtension: loadedClass forClass: aClass [
	^ self class registry_ImplementationClass registerLoadedClassExtension: loadedClass forClass: aClass
]

{ #category : 'class - registration' }
RwGsSymbolDictionaryRegistryV2 >> unregisterLoadedClassExtension: loadedClassExtension forClass: aClass [
	^ self class registry_ImplementationClass unregisterLoadedClassExtension: loadedClassExtension forClass: aClass
]
