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
	#category : 'Rowan-GemStone-Core'
}

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
RwGsSymbolDictionaryRegistry >> _doDeleteCompiledMethodFromLoadedThings: compiledMethod for: behavior [

	"remove a compiled method from loaded things"

	^ self class registry_ImplementationClass _doDeleteCompiledMethodFromLoadedThings: compiledMethod for: behavior instance: self

]

{ #category : 'method - private api' }
RwGsSymbolDictionaryRegistry >> _doDeleteCompiledMethodFromLoadedThings: compiledMethod for: behavior implementationClass: implementationClass [

	"remove a compiled method from loaded things"

	"Use for calls from classes in Rowan-GemStone-Loader package"

	^ implementationClass _doDeleteCompiledMethodFromLoadedThings: compiledMethod for: behavior instance: self

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

	^ self class registry_ImplementationClass addClassAssociation: assoc forClass: class toPackageNamed: packageName instance: self

]

{ #category : 'private' }
RwGsSymbolDictionaryRegistry >> addClassAssociation: assoc forClass: class toPackageNamed: packageName implementationClass: implementationClass [

	"Copy the name association to the correct 
        SymbolDictionary in the live SymbolList.
        Create a LoadedClass for the new class, add it to the defining LoadedPackage."

	"Use for calls from classes in Rowan-GemStone-Loader package"

	^ implementationClass addClassAssociation: assoc forClass: class toPackageNamed: packageName instance: self

]

{ #category : 'private' }
RwGsSymbolDictionaryRegistry >> addExtensionCompiledMethod: compiledMethod for: behavior protocol: protocolString toPackageNamed: packageName [

	^ self class registry_ImplementationClass addExtensionCompiledMethod: compiledMethod for: behavior protocol: protocolString toPackageNamed: packageName instance: self

]

{ #category : 'private' }
RwGsSymbolDictionaryRegistry >> addExtensionCompiledMethod: compiledMethod for: behavior protocol: protocolString toPackageNamed: packageName implementationClass: implementationClass [

	"Use for calls from classes in Rowan-GemStone-Loader package"

	^ implementationClass addExtensionCompiledMethod: compiledMethod for: behavior protocol: protocolString toPackageNamed: packageName instance: self

]

{ #category : 'private' }
RwGsSymbolDictionaryRegistry >> addExtensionSessionMethods: methDict catDict: catDict for: behavior toPackageNamed: packageName [

	"expecting only a single method to be in methDict/catDict"

	^ self class registry_ImplementationClass addExtensionSessionMethods: methDict catDict: catDict for: behavior toPackageNamed: packageName instance: self

]

{ #category : 'private' }
RwGsSymbolDictionaryRegistry >> addExtensionSessionMethods: methDict catDict: catDict for: behavior toPackageNamed: packageName implementationClass: implementationClass [

	"expecting only a single method to be in methDict/catDict"

	"Use for calls from classes in Rowan-GemStone-Loader package"

	^ implementationClass addExtensionSessionMethods: methDict catDict: catDict for: behavior toPackageNamed: packageName instance: self

]

{ #category : 'private' }
RwGsSymbolDictionaryRegistry >> addMovedDeletedMethod: compiledMethod for: behavior protocol: protocolString toPackageNamed: packageName [

	"there is an existing compiled method that has already been deleted from another package ... so we're adding it
		back using specialized processing"

	^ self class registry_ImplementationClass addMovedDeletedMethod: compiledMethod for: behavior protocol: protocolString toPackageNamed: packageName instance: self

]

{ #category : 'private' }
RwGsSymbolDictionaryRegistry >> addMovedDeletedMethod: compiledMethod for: behavior protocol: protocolString toPackageNamed: packageName implementationClass: implementationClass [

	"there is an existing compiled method that has already been deleted from another package ... so we're adding it
		back using specialized processing"

	"Use for calls from classes in Rowan-GemStone-Loader package"

	^ implementationClass addMovedDeletedMethod: compiledMethod for: behavior protocol: protocolString toPackageNamed: packageName instance: self

]

{ #category : 'private' }
RwGsSymbolDictionaryRegistry >> addNewClassVersionToAssociation: newClass [

	"a new class version is being added to the association in the receiver previously occupied by the original class"

	^ self class registry_ImplementationClass addNewClassVersionToAssociation: newClass instance: self

]

{ #category : 'private' }
RwGsSymbolDictionaryRegistry >> addNewClassVersionToAssociation: newClass implementationClass: implementationClass [

	"a new class version is being added to the association in the receiver previously occupied by the original class"

	"Use for calls from classes in Rowan-GemStone-Loader package"

	^ implementationClass addNewClassVersionToAssociation: newClass instance: self

]

{ #category : 'private' }
RwGsSymbolDictionaryRegistry >> addNewCompiledMethod: compiledMethod for: behavior protocol: protocolString toPackageNamed: packageName [

	^ self class registry_ImplementationClass addNewCompiledMethod: compiledMethod for: behavior protocol: protocolString toPackageNamed: packageName instance: self

]

{ #category : 'private' }
RwGsSymbolDictionaryRegistry >> addNewCompiledMethod: compiledMethod for: behavior protocol: protocolString toPackageNamed: packageName implementationClass: implementationClass [

	"Use for calls from classes in Rowan-GemStone-Loader package"

	^ implementationClass addNewCompiledMethod: compiledMethod for: behavior protocol: protocolString toPackageNamed: packageName instance: self

]

{ #category : 'private' }
RwGsSymbolDictionaryRegistry >> addRecompiledMethod: newCompiledMethod [

	"add a recompiled compiled method to behavior and update the loaded things"

	^ self class registry_ImplementationClass addRecompiledMethod: newCompiledMethod instance: self

]

{ #category : 'private' }
RwGsSymbolDictionaryRegistry >> addRecompiledMethod: newCompiledMethod implementationClass: implementationClass [

	"add a recompiled compiled method to behavior and update the loaded things"

	"Use for calls from classes in Rowan-GemStone-Loader package"

	^ implementationClass addRecompiledMethod: newCompiledMethod instance: self

]

{ #category : 'private' }
RwGsSymbolDictionaryRegistry >> addRecompiledSessionMethodMethod: newCompiledMethod [

	"add a recompiled session method compiled method to behavior and update the loaded things"

	^ self class registry_ImplementationClass addRecompiledSessionMethodMethod: newCompiledMethod instance: self

]

{ #category : 'private' }
RwGsSymbolDictionaryRegistry >> addRecompiledSessionMethodMethod: newCompiledMethod implementationClass: implementationClass [

	"add a recompiled session method compiled method to behavior and update the loaded things"

	"Use for calls from classes in Rowan-GemStone-Loader package"

	^ implementationClass addRecompiledSessionMethodMethod: newCompiledMethod instance: self

]

{ #category : 'method - adopt api' }
RwGsSymbolDictionaryRegistry >> adoptCompiledMethod: compiledMethod classExtension: classExtension for: behavior protocol: protocolString toPackageNamed: packageName [

	| methodDictionary selector protocolSymbol existing loadedMethod loadedPackage loadedClassOrExtension |
	classExtension
		ifTrue: [
			"to adopt and extension method, it is much less complicated" 
			^ self 
				addExtensionCompiledMethod: compiledMethod 
				for: behavior 
				protocol: protocolString 
				toPackageNamed: packageName ].

	methodDictionary := behavior persistentMethodDictForEnv: 0.
	selector := compiledMethod selector.
	compiledMethod == (methodDictionary at: selector ifAbsent: [ self error: 'expected an existing compiled method' ])
		ifFalse: [ self error: 'The given compiled method does not the existing compiled method in the class.' ].

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

{ #category : 'accessing' }
RwGsSymbolDictionaryRegistry >> classExtensionRegistry [

	^ classExtensionRegistry
]

{ #category : 'accessing' }
RwGsSymbolDictionaryRegistry >> classRegistry [

   ^classRegistry

]

{ #category : 'private' }
RwGsSymbolDictionaryRegistry >> createAndInstallLoadedClassForClass: class toPackageNamed: packageName [

	"Copy the name association to the correct 
        SymbolDictionary in the live SymbolList.
        Create a LoadedClass for the new class, add it to the defining LoadedPackage."

	^ self class registry_ImplementationClass createAndInstallLoadedClassForClass: class toPackageNamed: packageName instance: self

]

{ #category : 'private' }
RwGsSymbolDictionaryRegistry >> createAndInstallLoadedClassForClass: class toPackageNamed: packageName implementationClass: implementationClass [

	"Copy the name association to the correct 
        SymbolDictionary in the live SymbolList.
        Create a LoadedClass for the new class, add it to the defining LoadedPackage."

	"Use for calls from classes in Rowan-GemStone-Loader package"

	^ implementationClass createAndInstallLoadedClassForClass: class toPackageNamed: packageName instance: self

]

{ #category : 'package - patch api' }
RwGsSymbolDictionaryRegistry >> createLoadedPackageFromDefinition: packageDefinition [

	^ self class registry_ImplementationClass createLoadedPackageFromDefinition: packageDefinition instance: self

]

{ #category : 'package - patch api' }
RwGsSymbolDictionaryRegistry >> createLoadedPackageFromDefinition: packageDefinition implementationClass: implementationClass [

	"Use for calls from classes in Rowan-GemStone-Loader package"

	^ implementationClass createLoadedPackageFromDefinition: packageDefinition instance: self

]

{ #category : 'package - patch api' }
RwGsSymbolDictionaryRegistry >> createLoadedPackageNamed: packageName properties: properties [

	^ self class registry_ImplementationClass createLoadedPackageNamed: packageName properties: properties instance: self

]

{ #category : 'package - patch api' }
RwGsSymbolDictionaryRegistry >> createLoadedPackageNamed: packageName properties: properties  implementationClass: implementationClass [

	"Use for calls from classes in Rowan-GemStone-Loader package"

	^ implementationClass createLoadedPackageNamed: packageName properties: properties instance: self

]

{ #category : 'class - patch api' }
RwGsSymbolDictionaryRegistry >> deleteClassFromPackage: class [

	"a class association is being deleted from the receiver remove it from the loaded things"

	^ self class registry_ImplementationClass deleteClassFromPackage: class instance: self

]

{ #category : 'class - patch api' }
RwGsSymbolDictionaryRegistry >> deleteClassFromPackage: class implementationClass: implementationClass [

	"a class association is being deleted from the receiver remove it from the loaded things"

	"Use for calls from classes in Rowan-GemStone-Loader package"

	^ implementationClass deleteClassFromPackage: class instance: self

]

{ #category : 'class - patch api' }
RwGsSymbolDictionaryRegistry >> deleteClassNamedFromPackage: className [

	"a class association is being deleted from the receiver remove it from the loaded things"

	^ self class registry_ImplementationClass deleteClassNamedFromPackage: className instance: self

]

{ #category : 'class - patch api' }
RwGsSymbolDictionaryRegistry >> deleteClassNamedFromPackage: className implementationClass: implementationClass [

	"a class association is being deleted from the receiver remove it from the loaded things"

	"Use for calls from classes in Rowan-GemStone-Loader package"

	^ implementationClass deleteClassNamedFromPackage: className instance: self

]

{ #category : 'method - patch api' }
RwGsSymbolDictionaryRegistry >> deleteCompiledMethod: compiledMethod from: behavior [

	"delete a compiled method from behavior and remove it from the loaded things"

	^ self class registry_ImplementationClass deleteCompiledMethod: compiledMethod from: behavior instance: self

]

{ #category : 'method - patch api' }
RwGsSymbolDictionaryRegistry >> deleteCompiledMethod: compiledMethod from: behavior implementationClass: implementationClass [

	"delete a compiled method from behavior and remove it from the loaded things"

	"Use for calls from classes in Rowan-GemStone-Loader package"

	^ implementationClass deleteCompiledMethod: compiledMethod from: behavior instance: self

]

{ #category : 'method - patch api' }
RwGsSymbolDictionaryRegistry >> deleteMethod: selector for: behavior [

	^ self class registry_ImplementationClass deleteMethod: selector for: behavior instance: self

]

{ #category : 'method - patch api' }
RwGsSymbolDictionaryRegistry >> deleteMethod: selector for: behavior implementationClass: implementationClass [

	"Use for calls from classes in Rowan-GemStone-Loader package"

	^ implementationClass deleteMethod: selector for: behavior instance: self

]

{ #category : 'method - patch api' }
RwGsSymbolDictionaryRegistry >> deleteMovedMethod: selector for: behavior implementationClass: implementationClass [

	"Use for calls from classes in Rowan-GemStone-Loader package"

	^ implementationClass deleteMovedMethod: selector for: behavior instance: self

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

	^ self class registry_ImplementationClass ensureExtensionClassNamed: className existsForPackageNamed: packageName instance: self

]

{ #category : 'class - patch api' }
RwGsSymbolDictionaryRegistry >> ensureExtensionClassNamed: className existsForPackageNamed: packageName implementationClass: implementationClass [

	"Use for calls from classes in Rowan-GemStone-Loader package"

	^ implementationClass ensureExtensionClassNamed: className existsForPackageNamed: packageName instance: self

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

{ #category : 'package - creation api' }
RwGsSymbolDictionaryRegistry >> existingOrNewLoadedPackageNamed: packageName [

	^ self class registry_ImplementationClass existingOrNewLoadedPackageNamed: packageName instance: self

]

{ #category : 'package - creation api' }
RwGsSymbolDictionaryRegistry >> existingOrNewLoadedPackageNamed: packageName implementationClass: implementationClass [

	^ implementationClass existingOrNewLoadedPackageNamed: packageName instance: self

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

	^ self class registry_ImplementationClass loadedHybridPackageNamed: hybridPackageName ifAbsent: absentBlock instance: self

]

{ #category : 'loaded queries' }
RwGsSymbolDictionaryRegistry >> loadedHybridPackageNamed: hybridPackageName ifAbsent: absentBlock implementationClass: implementationClass [

	"Use for calls from classes in Rowan-GemStone-Loader package"

	^ implementationClass loadedHybridPackageNamed: hybridPackageName ifAbsent: absentBlock instance: self

]

{ #category : 'loaded queries' }
RwGsSymbolDictionaryRegistry >> loadedPackageNamed: packageName ifAbsent: absentBlock [

	^ self class registry_ImplementationClass loadedPackageNamed: packageName ifAbsent: absentBlock instance: self

]

{ #category : 'loaded queries' }
RwGsSymbolDictionaryRegistry >> loadedPackageNamed: packageName ifAbsent: absentBlock implementationClass: implementationClass [

	"Use for calls from classes in Rowan-GemStone-Loader package"

	^ implementationClass loadedPackageNamed: packageName ifAbsent: absentBlock instance: self

]

{ #category : 'accessing' }
RwGsSymbolDictionaryRegistry >> methodRegistry [

   ^methodRegistry

]

{ #category : 'method - patch api' }
RwGsSymbolDictionaryRegistry >> moveCompiledMethod: compiledMethod toProtocol: newProtocol [

	"move a compiled method into a different protocol and update loaded things"

	^ self class registry_ImplementationClass moveCompiledMethod: compiledMethod toProtocol: newProtocol instance: self

]

{ #category : 'method - patch api' }
RwGsSymbolDictionaryRegistry >> moveCompiledMethod: compiledMethod toProtocol: newProtocol implementationClass: implementationClass [

	"move a compiled method into a different protocol and update loaded things"

	"Use for calls from classes in Rowan-GemStone-Loader package"

	^ implementationClass moveCompiledMethod: compiledMethod toProtocol: newProtocol instance: self

]

{ #category : 'package - creation api' }
RwGsSymbolDictionaryRegistry >> newLoadedPackageNamed: packageName [

	^ self class registry_ImplementationClass newLoadedPackageNamed: packageName instance: self

]

{ #category : 'package - creation api' }
RwGsSymbolDictionaryRegistry >> newLoadedPackageNamed: packageName implementationClass: implementationClass [

	^ implementationClass newLoadedPackageNamed: packageName instance: self

]

{ #category : 'accessing' }
RwGsSymbolDictionaryRegistry >> packageRegistry [

   ^packageRegistry

]

{ #category : 'class - patch api' }
RwGsSymbolDictionaryRegistry >> updateClassProperties: class [

	"class properties have changed, update the clas properties of the loaded things"

	^ self class registry_ImplementationClass updateClassProperties: class instance: self

]

{ #category : 'class - patch api' }
RwGsSymbolDictionaryRegistry >> updateClassProperties: class  implementationClass: implementationClass [

	"class properties have changed, update the clas properties of the loaded things"

	"Use for calls from classes in Rowan-GemStone-Loader package"

	^ implementationClass updateClassProperties: class instance: self

]
