Class {
	#name : 'RwGsPatchSet_V2_symbolList',
	#superclass : 'RwGsPatchSet_V2',
	#instVars : [
		'tempSymbolList',
		'movedClassesSymbolList',
		'addedUnmanagedClasses',
		'loadSymbolList'
	],
	#category : 'Rowan-GemStone-LoaderV2'
}

{ #category : 'private - method initialization order' }
RwGsPatchSet_V2_symbolList class >> classPatchesInReverseHierarchyOrder: classPatches tempSymbolList: tempSymbolList [
	"Returns acollection of the specified classPatches ordered in reverse superclass order"

	| order toBeOrdered processed aClass patchMap |
	patchMap := IdentityKeyValueDictionary new.
	classPatches
		do: [ :classPatch | 
			| class |
			class := (self
				lookupSymbolDictName: classPatch symbolDictionaryName
				in: tempSymbolList)
				at: classPatch className asSymbol
				ifAbsent: [ 
					(tempSymbolList resolveSymbol: classPatch className asSymbol)
						ifNil: [ 
							"cannot find class ... caller can decide whether or not that is a problem"
							self error: 'Cannot find class to update constraints for.' ]
						ifNotNil: [ :assoc | assoc value ] ].
			patchMap at: class put: classPatch ].
	toBeOrdered := patchMap keys asIdentitySet.
	order := OrderedCollection new.
	processed := IdentitySet new.
	[ (aClass := self _anyElementOf: toBeOrdered ifEmpty: [ nil ]) isNil ]
		whileFalse: [ 
			self
				_orderBySuperclass: aClass
				from: toBeOrdered
				into: order
				ignoring: processed ].
	^ ((order collect: [ :orderedClass | patchMap at: orderedClass ifAbsent: [  ] ])
		select: [ :patch | patch notNil ]) reverse
]

{ #category : 'accessing' }
RwGsPatchSet_V2_symbolList class >> lookupSymbolDictName: symDictName in: symbolList [
	^ self
		lookupSymbolDictName: symDictName
		in: symbolList
		ifAbsent: [ 
			self
				error:
					'the symbol dictionary named ' , symDictName asString printString
						, ' was not found in the symbol list' ]
]

{ #category : 'accessing' }
RwGsPatchSet_V2_symbolList class >> lookupSymbolDictName: symDictName in: symbolList ifAbsent: absentBlock [
	^ symbolList
		detect: [ :each | (each at: symDictName ifAbsent: [ nil ]) == each ]
		ifNone: absentBlock
]

{ #category : 'accessing' }
RwGsPatchSet_V2_symbolList class >> resolveSymbolDictWith: assocation in: symbolList [
| res aDict |
res := { } .
1 to: symbolList size do:[ :j |
  (aDict := symbolList at: j) ifNotNil:[
    aDict associationsDo:[ :assoc |
      assoc == assocation ifTrue:[ res add: aDict]
    ].
  ].
].
^ res
]

{ #category : 'private - patch class accessors' }
RwGsPatchSet_V2_symbolList >> _classUnmanagedVersioningPatchClass [

	^ RwGsClassUnmanagedVersioningSymbolDictPatchV2
]

{ #category : 'private - applying' }
RwGsPatchSet_V2_symbolList >> _createMovedClasses [
	movedClasses
		do: [ :movedClass | 
			| symDictName |
			symDictName := movedClass symbolDictionaryNameBefore asSymbol.
			( self class  lookupSymbolDictName: symDictName in: self movedClassesSymbolList)
				at: movedClass classBefore name asSymbol
				put: movedClass ]
]

{ #category : 'private - applying' }
RwGsPatchSet_V2_symbolList >> _createNewSymbolList [
	| new |
	new := SymbolList new.
	self loadSymbolList
		do: [ :symDict | 
			| newSymDict |
			newSymDict := SymbolDictionary new
				name: symDict name;
				yourself.
			new addLast: newSymDict ].
	^ new
]

{ #category : 'building' }
RwGsPatchSet_V2_symbolList >> addAddedUnmanagedClass: aClassDefinition oldClassVersion: aClass inPackage: aPackageDefinition inProject: aProjectDefinition [

	currentProjectDefinition := aProjectDefinition.
	addedUnmanagedClasses
		add:
			((self _classUnmanagedAdditionPatchClass
				for: aClassDefinition
				inPackage: aPackageDefinition)
				projectDefinition: aProjectDefinition;
				oldClassVersion: aClass
				yourself)
]

{ #category : 'modification dispatching' }
RwGsPatchSet_V2_symbolList >> addClassModification: aRwClassModification toPatchSetInPackage: aPackage inProject: aProjectDefinition [
	"Double dispatch from aRwClassModification ... needed to isolate the loader methods from meaningful changes 
		while updating the loader using the loader"

	aRwClassModification isAddition
		ifTrue: [ 
			| symDictName className |
			"https://github.com/dalehenrich/Rowan/issues/210 - make sure that the added classes are not already loaded
				in a project that is not included in this load"
			className := aRwClassModification after name.
			symDictName := aProjectDefinition
				symbolDictNameForPackageNamed: aPackage name.
			Rowan image newOrExistingSymbolDictionaryNamed: symDictName.
			(Rowan globalNamed: aRwClassModification after name)
				ifNotNil: [ :class | 
					(Rowan image loadedClassForClass: class ifAbsent: [  ])
						ifNil: [ 
							| theClassDefinition theClassModification |
							"no loaded class exists for the class"
							theClassDefinition := class
								rwClassDefinitionInSymbolDictionaryNamed: symDictName.
							theClassModification := aRwClassModification after
								compareAgainstBase: theClassDefinition.
							theClassModification isEmpty
								ifTrue: [ 
									self
										addAddedUnmanagedClass: aRwClassModification after
										oldClassVersion: class
										inPackage: aPackage
										inProject: aProjectDefinition ]
								ifFalse: [ 
									aRwClassModification before: theClassDefinition.
									self
										addPatchedUnmanagedClassNewVersion: aRwClassModification
										inPackage: aPackage
										inProject: aProjectDefinition ].
							(aRwClassModification propertiesModification elementsModified
								at: 'gs_constraints'
								ifAbsent: [  ])
								ifNotNil: [ :constraints | 
									"arrange to add constraints to a newly created class - constraints not created during class creation"
									self
										addPatchedClassConstraints: aRwClassModification after
										inPackage: aPackage
										inProject: aProjectDefinition ].
							^ self ]
						ifNotNil: [ 
							"if the class is packaged, then it must be in another project, signal notification"
							(RwExistingVisitorAddingExistingClassNotification new
								class: class;
								classDefinition: aRwClassModification after;
								yourself) signal ] ].
			self
				addAddedClass: aRwClassModification after
				inPackage: aPackage
				inProject: aProjectDefinition.
			(aRwClassModification propertiesModification elementsModified
				at: 'gs_constraints'
				ifAbsent: [  ])
				ifNotNil: [ :constraints | 
					"arrange to add constraints to a newly created class - constraints not created during class creation"
					self
						addPatchedClassConstraints: aRwClassModification after
						inPackage: aPackage
						inProject: aProjectDefinition ].
			^ self ].
	aRwClassModification isDeletion
		ifTrue: [ 
			self
				addDeletedClass: aRwClassModification before
				inPackage: aPackage
				inProject: aProjectDefinition ].
	aRwClassModification isModification
		ifTrue: [ 
			RwGsClassVersioningPatchV2
				addPatchedClassModification: aRwClassModification
				inPackage: aPackage
				inProject: aProjectDefinition
				toPatchSet: self.
			(aRwClassModification propertiesModification elementsModified
				at: 'gs_constraints'
				ifAbsent: [  ])
				ifNotNil: [ :constraints | 
					"arrange to add constraints to a newly created class - constraints not created during class creation"
					self
						addPatchedClassConstraints: aRwClassModification after
						inPackage: aPackage
						inProject: aProjectDefinition ] ]
]

{ #category : 'private - applying' }
RwGsPatchSet_V2_symbolList >> addCreatedClassesAndVersionsToSymbolList: newClassesByNameSymbolList [
	addedClasses
		do: [ :patch | patch addToNewClassesByNameSymbolList: newClassesByNameSymbolList ].
	addedUnmanagedClasses
		do: [ :patch | patch addToNewClassesByNameSymbolList: newClassesByNameSymbolList ].
	classesWithClassVariableChanges
		do: [ :patch | patch addToNewClassesByNameSymbolList: newClassesByNameSymbolList ].
	classesWithNewVersions
		do: [ :patch | patch addToNewClassesByNameSymbolList: newClassesByNameSymbolList ]
]

{ #category : 'building' }
RwGsPatchSet_V2_symbolList >> addPatchedUnmanagedClassNewVersion: aClassModification inPackage: aPackageDefinition inProject: aProjectDefinition [

	currentProjectDefinition := aProjectDefinition.

	classesWithNewVersions
		add:
			((self _classUnmanagedVersioningPatchClass
				for: aClassModification
				inPackage: aPackageDefinition)
				projectDefinition: aProjectDefinition;
				yourself)
]

{ #category : 'private - applying' }
RwGsPatchSet_V2_symbolList >> compileMethodPatch: aMethodAdditionPatch [
	aMethodAdditionPatch
		compileUsingNewClassesSymbolList: self createdClasses
		andExistingClasses: self tempSymbolList
]

{ #category : 'private - applying' }
RwGsPatchSet_V2_symbolList >> createClassesFromWorkSymbolList: workSymbolList symDictName: symDictName andClassesByNameSymbolList: newClassesByNameSymbolList [
	"Pick one class or class version from the workList and create it, creating any superclasses or superclass versions that are to be created.
	Remove any classes created from the workList."

	| classNames className |
	classNames := (self class lookupSymbolDictName: symDictName in: workSymbolList)
		keys.
	classNames remove: symDictName.
	className := self anyElementOfCollection: classNames.
	className ifNil: [ self error: 'Empty WorkList.' ].
	self
		createClassNamed: className
		fromWorkSymbolList: workSymbolList
		symDictName: symDictName
		andClassesByNameSymbolList: newClassesByNameSymbolList
]

{ #category : 'private - applying' }
RwGsPatchSet_V2_symbolList >> createClassNamed: className fromWorkSymbolList: workSymbolList symDictName: symDictName andClassesByNameSymbolList: newClassesByNameSymbolList [
	"Create the named class from the workList, creating any superclasses or superclass versions that are to be created.
	Remove any classes created from the workList."

	| patch superclassName |
	(self class lookupSymbolDictName: symDictName in: workSymbolList)
		removeKey: className asSymbol.
	patch := (self class
		lookupSymbolDictName: symDictName
		in: newClassesByNameSymbolList) at: className asSymbol.
	superclassName := patch superclassName asSymbol.
	(workSymbolList resolveSymbol: superclassName)
		ifNotNil: [ :superclassAssoc | 
			| ar |
			ar := self class resolveSymbolDictWith: superclassAssoc in: workSymbolList.
			ar size ~= 1
				ifTrue: [ 
					self
						error:
							'Internal error: more than one symbol dictionary with association for class '
								, superclassName printString ].
			self
				createClassNamed: superclassName
				fromWorkSymbolList: workSymbolList
				symDictName: (ar at: 1) name
				andClassesByNameSymbolList: newClassesByNameSymbolList ].
	patch createClassFor: self inSymDict: symDictName
]

{ #category : 'patch access' }
RwGsPatchSet_V2_symbolList >> createdClass: aClass [
	| className |
	className := aClass name asSymbol.
	(createdClasses resolveSymbol: className)
		ifNil: [ createdClasses add: (SymbolAssociation newWithKey: className value: aClass) ]
		ifNotNil: [ :assoc | 
			assoc value == aClass
				ifFalse: [ 
					"new version created, update entry in createdClasses"
					createdClasses at: className put: aClass ] ]
]

{ #category : 'patch access' }
RwGsPatchSet_V2_symbolList >> createdClass: aClass inSymDict: symDictName [
	| className symDict |
	className := aClass name.
	symDict := self class lookupSymbolDictName: symDictName in: self createdClasses.
	(symDict at: className ifAbsent: [  ])
		ifNil: [ symDict add: (SymbolAssociation newWithKey: className value: aClass) ]
		ifNotNil: [ :cl | 
			cl == aClass
				ifFalse: [ 
					"new version created, update entry in createdClasses"
					symDict at: className put: aClass ] ]
]

{ #category : 'accessing' }
RwGsPatchSet_V2_symbolList >> createdClasses [
	^ createdClasses ifNil: [ createdClasses :=  self _createNewSymbolList ]
]

{ #category : 'private - applying' }
RwGsPatchSet_V2_symbolList >> createNewClassesAndClassVersions [
	"Who: Added classes and classes with new versions and classes with class variable changes
	   How: Create the new class or class version. Superclass by identity, not name resolution.
	   Order: Superclasses first.
	   Errors: Collect for reporting later"

	| newClassesByNameSymbolList workSymbolList |
	newClassesByNameSymbolList := self _createNewSymbolList.
	self addCreatedClassesAndVersionsToSymbolList: newClassesByNameSymbolList.
	workSymbolList := self _createNewSymbolList.
	newClassesByNameSymbolList
		do: [ :symDict | 
			| workList symDictName |
			symDictName := symDict name.
			workList := symDict keys.
			workList remove: symDictName.
			workList
				do: [ :className | 
					(self class lookupSymbolDictName: symDictName in: workSymbolList)
						at: className
						put: nil ] ].
	workSymbolList
		do: [ :symDict | 
			[ symDict keys size <= 1 ]
				whileFalse: [ 
					self
						createClassesFromWorkSymbolList: workSymbolList
						symDictName: symDict name
						andClassesByNameSymbolList: newClassesByNameSymbolList ] ]
]

{ #category : 'private - applying' }
RwGsPatchSet_V2_symbolList >> doMoveMethodsBetweenPackages [

	| image |
	image := Rowan image.
	movedMethods 
		do: [:aMethodMove |
			| packageDef classOrExtensionDef loadedPackage loadedClassOrExtension loadedMethods loadedMethod compiledMethod registry |
			packageDef := aMethodMove packageBefore.
			classOrExtensionDef := aMethodMove classOrExtensionBefore.
			loadedPackage := image loadedPackageNamed: packageDef name.
			loadedClassOrExtension := loadedPackage 
				classOrExtensionForClassNamed: classOrExtensionDef name 
				ifAbsent: [ 
					self error: 'internal error - no loaded class or extension found for class ', 
						classOrExtensionDef name printString, 
						'  in package ', 
						packageDef name printString ].
			loadedMethods := aMethodMove isMeta
				ifTrue: [ loadedClassOrExtension loadedClassMethods ]
				ifFalse: [ loadedClassOrExtension loadedInstanceMethods ].
			loadedMethod := loadedMethods at: aMethodMove methodBefore selector.

			registry := image loadedRegistryForPackageNamed: loadedPackage name.

			loadedClassOrExtension removeLoadedMethod: loadedMethod.
			loadedClassOrExtension isLoadedClassExtension
				ifTrue: [
					loadedClassOrExtension isEmpty
						ifTrue: [ 
							(registry loadedClassExtensionsForClass: loadedClassOrExtension handle ifAbsent: [])
								ifNotNil: [ :loadedClassExtensions |
									loadedClassExtensions isEmpty
										ifFalse: [ 
											loadedClassExtensions remove: loadedClassOrExtension.
											loadedClassExtensions isEmpty
												ifTrue: [ 
													registry 
														unregisterLoadedClassExtension: loadedClassOrExtension
														forClass: loadedClassOrExtension handle ] ] ].
							loadedPackage removeLoadedClassExtension: loadedClassOrExtension ] ].

            compiledMethod := loadedMethod handle.
			registry methodRegistry removeKey: compiledMethod ifAbsent: [].

			packageDef := aMethodMove packageAfter.
			classOrExtensionDef := aMethodMove classOrExtensionAfter.
			loadedPackage := image loadedPackageNamed: packageDef name.
			registry := (Rowan image symbolDictNamed: (aMethodMove projectAfter  symbolDictNameForPackageNamed: aMethodMove packageAfter name)) rowanSymbolDictionaryRegistry.
			loadedClassOrExtension := loadedPackage 
				classOrExtensionForClassNamed: classOrExtensionDef name 
				ifAbsent: [ 
					classOrExtensionDef isClassExtension
						ifTrue: [ 
							"add a new class extension on demand"
							(aMethodMove projectAfter
								useSessionMethodsForExtensionsForPackageNamed: aMethodMove packageAfter name)
								ifTrue: [ | resolved |
									((resolved := self loadSymbolList objectNamed: classOrExtensionDef name asSymbol) notNil 
										and: [resolved isBehavior and: [ resolved isMeta not ]])
										ifFalse: [ 
											self
												error:
													'The extension class named ' , self className printString , ' cannot be found.' ].
"logic is not quite correct here ... need to create a lodedClassExtension for this puppy ... deferring session method work to a later date"
"https://github.com/dalehenrich/Rowan/issues/254"]
								ifFalse: [
									registry
										ensureExtensionClassNamed: classOrExtensionDef name
										existsForPackageNamed: packageDef name
										implementationClass: RwGsSymbolDictionaryRegistry_ImplementationV2 ].
							loadedPackage loadedClassExtensions at: classOrExtensionDef name ]
						ifFalse: [ 
							self error: 'internal error - no loaded class found for class ', 
								classOrExtensionDef name printString, 
								'  in package ', 
								packageDef name printString ] ].

			loadedClassOrExtension addLoadedMethod: loadedMethod.

			registry methodRegistry at: compiledMethod put: loadedMethod ]
]

{ #category : 'initialization' }
RwGsPatchSet_V2_symbolList >> initialize [
	addedPackages := Set new.
	addedProjects := Set new.
	deletedPackages := Set new.
	movedPackages := Set new.
	projectsWithPropertyChanges := Set new.
	deletedClasses := Set new.
	addedClasses := Set new.
	extendedClasses := Set new.
	classesWithNewVersions := Set new.
	classesWithClassVariableChanges := Set new.
	classesWithPropertyChanges := Set new.
	classesWithConstraintChanges := Set new.
	classesWithSymbolDictionaryChanges := Set new.
	movedClasses := Set new.
	movedClassesMap := Dictionary new.
	deletedMethods := Set new.
	deleteNewVersionMethods := Set new.
	movedMethods := Set new.
	addedMethods := Set new.
	extendedMethods := Set new.
	methodsNeedingRecompile := Set new.
	methodsWithPropertyChanges := Set new.
	tempSymbols := SymbolDictionary new.
	createdClasses := Dictionary new.
	errors := Set new.
	createdClasses := nil.
	addedUnmanagedClasses := Set new
]

{ #category : 'private - applying' }
RwGsPatchSet_V2_symbolList >> installAddedClasses [
	"Copy the name association from TempSymbols to the correct 
        SymbolDictionary in the live SymbolList.
        Create a LoadedClass for the new class, add it to the defining LoadedPackage."

	addedClasses do: [:patch |  patch installClassInSystem ].
	addedUnmanagedClasses do: [ :patch | patch installClassInSystem ]
]

{ #category : 'private - applying' }
RwGsPatchSet_V2_symbolList >> installAddedClassExtensions [

	extendedClasses do: [ :each | each installClassExtensionInSystem: self loadSymbolList ]
]

{ #category : 'accessing' }
RwGsPatchSet_V2_symbolList >> loadSymbolList [
	^ loadSymbolList ifNil: [ Rowan image symbolList ]
]

{ #category : 'accessing' }
RwGsPatchSet_V2_symbolList >> loadSymbolList: aSymbolList [
	loadSymbolList := aSymbolList
]

{ #category : 'private - applying' }
RwGsPatchSet_V2_symbolList >> moveClassesBetweenSymbolDictionaries [
	classesWithSymbolDictionaryChanges
		do: [ :patch | 
			| className |
			className := patch classDefinition name asSymbol.
			(self movedClassesSymbolList resolveSymbol: className)
				ifNil: [ patch installSymbolDictionaryPatchFor: self ]
				ifNotNil: [ :assoc | 
					| classMove |
					classMove := assoc value.
					patch installSymbolDictionaryPatchFor: self classMove: classMove ] ]
]

{ #category : 'accessing' }
RwGsPatchSet_V2_symbolList >> movedClassesMap [
	self error: 'Use movedClassedSymbolList'
]

{ #category : 'accessing' }
RwGsPatchSet_V2_symbolList >> movedClassesSymbolList [
	^ movedClassesSymbolList
		ifNil: [ movedClassesSymbolList := self _createNewSymbolList ]
]

{ #category : 'private - applying' }
RwGsPatchSet_V2_symbolList >> recordCompiledForNewClassVersionDeletions [
	deleteNewVersionMethods
		do: [ :each | 
			each
				primeBehaviorNewClassesSymbolList: self createdClasses
				andExistingClasses: self tempSymbols ]
]

{ #category : 'private - applying' }
RwGsPatchSet_V2_symbolList >> removeDeletedMethods [
	deletedMethods
		do: [ :methodDeletionPatch | 
			| className |
			className := methodDeletionPatch className.
			methodDeletionPatch
				deleteMethodNewClassesSymbolList: self createdClasses
				andExistingClasses: self tempSymbolList ].
	deleteNewVersionMethods
		do: [ :methodDeletionPatch | 
			methodDeletionPatch
				deleteNewVersionMethodNewClassesSymbolList: self createdClasses
				andExistingClasses: self tempSymbolList ]
]

{ #category : 'patch access' }
RwGsPatchSet_V2_symbolList >> superclassNamed: aName ifAbsent: absentBlock [
	| superclassName |
	superclassName := aName asSymbol.
	^ (self createdClasses resolveSymbol: superclassName)
		ifNotNil: [ :assoc | assoc value ]
		ifNil: [ self tempSymbolList at: superclassName ifAbsent: absentBlock ]
]

{ #category : 'patch access' }
RwGsPatchSet_V2_symbolList >> tempAssociationFor: aName [
	^ self tempSymbolList associationAt: aName
]

{ #category : 'accessing' }
RwGsPatchSet_V2_symbolList >> tempSymbolList [
	^ tempSymbols
]

{ #category : 'private - applying' }
RwGsPatchSet_V2_symbolList >> updateClassProperties [
	"For classes with changes that don't require versioning, 
	update the properties in the class and the LoadedClasses as appropriate."

	| classPatches ts |
	ts := self tempSymbolList.
	classPatches := OrderedCollection new.
	classPatches
		addAll:
				(self class
						classPatchesInReverseHierarchyOrder: classesWithClassVariableChanges
						tempSymbols: ts);
		addAll:
				(self class
						classPatchesInReverseHierarchyOrder: classesWithPropertyChanges
						tempSymbols: ts);
		addAll:
				(self class
						classPatchesInReverseHierarchyOrder: classesWithConstraintChanges
						tempSymbols: ts);
		yourself.
	classPatches
		do: [ :patch | 
			((self class
				lookupSymbolDictName: patch symbolDictionaryName
				in: self movedClassesSymbolList) at: patch className asSymbol ifAbsent: [  ])
				ifNil: [ patch installPropertiesPatchSymbolListFor: self ]
				ifNotNil: [ :aClassMove | patch installPropertiesPatchSymbolListFor: self classMove: aClassMove ] ]
]

{ #category : 'private - applying' }
RwGsPatchSet_V2_symbolList >> updateMethodDictionaries [
	"For added methods and methods that need recompiling,
	copy from temporary methodDictionaries to live methodDictionaries
	For added methods, create LoadedMethods and add to the appropriate LoadedClass
	or LoadedExtension
	For methods that need recompiling updated the LoadedMethods registration. "

	| movedClassesWithNewVersionMap movedExtensionMethodsMap emptyMap |
	movedClassesWithNewVersionMap := self _createNewSymbolList.
	emptyMap := true.
	classesWithNewVersions
		do: [ :patch | 
			((self class
				lookupSymbolDictName: patch symbolDictionaryName
				in: self movedClassesSymbolList) at: patch className asSymbol ifAbsent: [  ])
				ifNotNil: [ :aClassMove | 
					(self class
						lookupSymbolDictName: aClassMove symbolDictionaryNameAfter asSymbol
						in: movedClassesWithNewVersionMap)
						at: patch className asSymbol
						put:
							{aClassMove.
							patch}.
					emptyMap := false ] ].
	addedMethods
		do: [ :patch | 
			| className |
			className := patch className asSymbol.
			((self class
				lookupSymbolDictName: patch symbolDictionaryName
				in: movedClassesWithNewVersionMap) at: className ifAbsent: [  ])
				ifNil: [ patch installMethod ]
				ifNotNil: [ :ar | 
					"https://github.com/dalehenrich/Rowan/issues/316"
					patch installMovedMethod: (ar at: 1) newClassVersionPatch: (ar at: 2) ] ].
	emptyMap
		ifFalse: [ 
			"calculate moved extension methods map only if there are moved new class versions as well"
			movedExtensionMethodsMap := self _createNewSymbolList.
			movedMethods
				do: [ :aMethodMove | 
					| classExtDict key methodDict |
					classExtDict := (self class
						lookupSymbolDictName: aMethodMove symbolDictionaryNameAfter asSymbol
						in: movedExtensionMethodsMap)
						at: aMethodMove classOrExtensionBefore name asSymbol
						ifAbsentPut: [ Dictionary new ].
					key := aMethodMove isMeta
						ifTrue: [ 'class' ]
						ifFalse: [ 'instance' ].
					methodDict := classExtDict at: key ifAbsentPut: [ Dictionary new ].
					methodDict at: aMethodMove methodAfter selector put: aMethodMove ] ].
	extendedMethods
		do: [ :patch | 
			| className |
			className := patch className asSymbol.
			(movedClassesWithNewVersionMap resolveSymbol: className)
				ifNil: [ patch installMethod ]
				ifNotNil: [ :movedAssoc | 
					| ar classMove |
					"https://github.com/dalehenrich/Rowan/issues/316"
					ar := movedAssoc value.
					classMove := ar at: 1.
					((self class
						lookupSymbolDictName: classMove symbolDictionaryNameAfter
						in: movedExtensionMethodsMap) at: className ifAbsent: [  ])
						ifNil: [ patch installMethod ]
						ifNotNil: [ :assoc | 
							| classExtDict methodDict |
							classExtDict := assoc value.
							methodDict := patch isMeta
								ifTrue: [ classExtDict at: 'class' ifAbsent: [ Dictionary new ] ]
								ifFalse: [ classExtDict at: 'instance' ifAbsent: [ Dictionary new ] ].
							(methodDict at: patch methodDefinition selector ifAbsent: [  ])
								ifNil: [ patch installMethod ]
								ifNotNil: [ :aMethodMove | patch installMovedMethod: aMethodMove newClassVersionPatch: (ar at: 2) ] ] ] ].
	methodsNeedingRecompile do: [ :each | each installSourcePatch ]
]

{ #category : 'private - applying' }
RwGsPatchSet_V2_symbolList >> updateMethodProperties [
	methodsWithPropertyChanges
		do: [ :each | 
			each
				installPropertiesPatchNewClassesSymbolList: self createdClasses
				andExistingClasses: self tempSymbols ]
]

{ #category : 'dispatching' }
RwGsPatchSet_V2_symbolList >> updatePatchesForNewClassVersion: aClassVersioningPatch projectModification: aProjectSetModification [
	aClassVersioningPatch
		updatePatchesForNewClassVersion: aProjectSetModification
			patchSetSymbolList: self;
		updateNewClassVersionPatchesForExtensionsIn: aProjectSetModification
			patchSet: self;
		updateNewClassVersionPatchesForSubclassesIn: aProjectSetModification
			patchSet: self
]

{ #category : 'private - applying' }
RwGsPatchSet_V2_symbolList >> updateSymbolAssociations [
	"Install new class versions."

	classesWithNewVersions
		do: [ :each | 
			(self movedClassesSymbolList resolveSymbol: each className asSymbol)
				ifNil: [ each installNewClassVersionInSystem ]
				ifNotNil: [ :assoc | 
					| classMove |
					classMove := assoc value.
					each moveNewClassVersionInSystem: classMove ] ]
]
