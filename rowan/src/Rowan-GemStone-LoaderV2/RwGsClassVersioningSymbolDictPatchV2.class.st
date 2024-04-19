Class {
	#name : 'RwGsClassVersioningSymbolDictPatchV2',
	#superclass : 'RwGsClassPatchV2',
	#instVars : [
		'classModification',
		'oldClassVersion',
		'newClassVersion',
		'symbolAssociation'
	],
	#category : 'Rowan-GemStone-LoaderV2'
}

{ #category : 'instance creation' }
RwGsClassVersioningSymbolDictPatchV2 class >> for: aClassModification inPackage: aPackageDefinition [

	^ self new
		classModification: aClassModification;
		packageDefinition: aPackageDefinition;
		yourself
]

{ #category : 'private' }
RwGsClassVersioningSymbolDictPatchV2 >> _createMethodAdditionModificationForNewVersionOfClassNamed: className isMeta: isMeta methodMove: aMethodMove projectSetModification: aProjectSetModification [
	| projectModification packageModification classesModification methodsModification methodDef source sourceModification modification afterMethodDef beforeMethodDef |
	projectModification := aProjectSetModification elementsModified
		at: aMethodMove projectAfter name
		ifAbsentPut: [ 
			| def |
			def := aMethodMove projectAfter.
			(RwProjectModification before: def after: def)
				propertiesModification: RwPropertiesModification new;
				packagesModification: RwPackagesModification new;
				yourself ].
	packageModification := projectModification packagesModification
		elementsModified
		at: aMethodMove packageAfter name
		ifAbsentPut: [ 
			| def |
			"arrange for a new package to be added"
			def := aMethodMove packageAfter.
			(RwPackageModification before: def after: def)
				classesModification: RwClassesModification new;
				classExtensionsModification: RwClassExtensionsModification new;
				yourself ].
	classesModification := packageModification classesModification elementsModified
		at: className
		ifAbsentPut: [ 
			| def |
			def := aMethodMove classOrExtensionAfter.
			(RwClassModification before: RwClassDefinition new after: def)
				instanceMethodsModification: RwMethodsModification new;
				classMethodsModification: RwMethodsModification new;
				yourself ].
	methodsModification := isMeta
		ifTrue: [ classesModification classMethodsModification elementsModified ]
		ifFalse: [ classesModification instanceMethodsModification elementsModified ].
	(methodsModification at: aMethodMove methodAfter selector ifAbsent: [  ])
		ifNil: [ 
			afterMethodDef := beforeMethodDef := methodDef := aMethodMove methodAfter.
			source := methodDef source.
			sourceModification := RwSourceModification new
				addElementModification:
						(RwUnconditionalPropertyModification
								key: 'source'
								oldValue: source
								newValue: source);
				yourself.
			modification := (RwMethodModificationForNewClassVersion
				before: beforeMethodDef
				after: afterMethodDef)
				isMeta: isMeta;
				classDefinition: aMethodMove classOrExtensionAfter;
				propertiesModification: RwPropertiesModification new;
				sourceModification: sourceModification;
				yourself ]
		ifNotNil: [ :theMethodModification | 
			modification := (RwMethodModificationForNewClassVersion
				before: theMethodModification before
				after: theMethodModification after)
				isMeta: isMeta;
				classDefinition: aMethodMove classOrExtensionAfter;
				propertiesModification: theMethodModification propertiesModification;
				sourceModification: theMethodModification sourceModification;
				yourself ].

	methodsModification at: aMethodMove methodAfter selector put: modification
]

{ #category : 'private' }
RwGsClassVersioningSymbolDictPatchV2 >> _createMethodExtensionModificationForNewVersionOfClassNamed: className isMeta: isMeta loadedMethod: loadedMethod projectSetModification: aProjectSetModification [

	| loadedProject loadedPackage loadedClassExtension projectModification packageModification 
		classExtensionModification methodsModification methodDef source sourceModification modification
		afterMethodDef beforeMethodDef |
	loadedClassExtension := loadedMethod loadedClass.
	loadedPackage := loadedMethod loadedPackage.
	loadedProject := loadedMethod loadedProject.
	projectModification := aProjectSetModification elementsModified
		at: loadedProject name
		ifAbsentPut: [
			| def |
			def := loadedProject asDefinition.
			(RwProjectModification before: def after: def)
				propertiesModification: RwPropertiesModification new;
				packagesModification: RwPackagesModification new;
				yourself ].
	packageModification := projectModification packagesModification
		elementsModified
		at: loadedPackage name
		ifAbsentPut: [ 
			| def |
			"arrange for a new package to be added"
			def := loadedPackage asDefinition.
			(RwPackageModification before: def after: def)
				classesModification: RwClassesModification new;
				classExtensionsModification: RwClassExtensionsModification new;
				yourself ].
	classExtensionModification := packageModification classExtensionsModification
		elementsModified
		at: className
		ifAbsentPut: [ 
			| def |
			def := (loadedPackage loadedClassExtensions at: className) asDefinition.
			(RwClassExtensionModification
				before: RwClassExtensionDefinition new
				after: def)
				instanceMethodsModification:
						(RwExtensionMethodsModification extendedClassName: className);
				classMethodsModification:
						(RwExtensionMethodsModification extendedClassName: className);
				yourself ].
	methodsModification := isMeta
		ifTrue: [ classExtensionModification classMethodsModification elementsModified ]
		ifFalse: [ classExtensionModification instanceMethodsModification elementsModified ].
	(methodsModification at: loadedMethod selector ifAbsent: [  ])
		ifNil: [
			afterMethodDef := beforeMethodDef := methodDef := loadedMethod asDefinition.
			source := methodDef source.
			sourceModification := RwSourceModification new
				addElementModification:
						(RwUnconditionalPropertyModification
								key: 'source'
								oldValue: source
								newValue: source);
				yourself.
			modification := (RwExtensionMethodModificationForNewClassVersion
				before: beforeMethodDef
				after: afterMethodDef)
				isMeta: isMeta;
				classDefinition: loadedClassExtension asDefinition;
				propertiesModification: RwPropertiesModification new;
				sourceModification: sourceModification;
				yourself ]
		ifNotNil: [:theMethodModification |
			modification := (RwExtensionMethodModificationForNewClassVersion
				before: theMethodModification before
				after: theMethodModification after)
				isMeta: isMeta;
				classDefinition: loadedClassExtension asDefinition;
				propertiesModification: theMethodModification propertiesModification;
				sourceModification: theMethodModification sourceModification;
				yourself ].

	methodsModification at: loadedMethod selector put: modification

]

{ #category : 'private' }
RwGsClassVersioningSymbolDictPatchV2 >> _createMethodExtensionModificationForNewVersionOfClassNamed: className isMeta: isMeta methodMove: aMethodMove projectSetModification: aProjectSetModification [
	| projectModification packageModification classExtensionModification methodsModification methodDef source sourceModification modification afterMethodDef beforeMethodDef |
	projectModification := aProjectSetModification elementsModified
		at: aMethodMove projectAfter name
		ifAbsentPut: [ 
			| def |
			def := aMethodMove projectAfter.
			(RwProjectModification before: def after: def)
				propertiesModification: RwPropertiesModification new;
				packagesModification: RwPackagesModification new;
				yourself ].
	packageModification := projectModification packagesModification
		elementsModified
		at: aMethodMove packageAfter name
		ifAbsentPut: [ 
			| def |
			"arrange for a new package to be added"
			def := aMethodMove packageAfter.
			(RwPackageModification before: def after: def)
				classesModification: RwClassesModification new;
				classExtensionsModification: RwClassExtensionsModification new;
				yourself ].
	classExtensionModification := packageModification classExtensionsModification
		elementsModified
		at: className
		ifAbsentPut: [ 
			| def |
			def := aMethodMove classOrExtensionAfter.
			(RwClassExtensionModification
				before: RwClassExtensionDefinition new
				after: def)
				instanceMethodsModification:
						(RwExtensionMethodsModification extendedClassName: className);
				classMethodsModification:
						(RwExtensionMethodsModification extendedClassName: className);
				yourself ].
	methodsModification := isMeta
		ifTrue: [ classExtensionModification classMethodsModification elementsModified ]
		ifFalse: [ classExtensionModification instanceMethodsModification elementsModified ].
	(methodsModification at: aMethodMove methodAfter selector ifAbsent: [  ])
		ifNil: [ 
			afterMethodDef := beforeMethodDef := methodDef := aMethodMove methodAfter.
			source := methodDef source.
			sourceModification := RwSourceModification new
				addElementModification:
						(RwUnconditionalPropertyModification
								key: 'source'
								oldValue: source
								newValue: source);
				yourself.
			modification := (RwExtensionMethodModificationForNewClassVersion
				before: beforeMethodDef
				after: afterMethodDef)
				isMeta: isMeta;
				classDefinition: aMethodMove classOrExtensionAfter;
				propertiesModification: RwPropertiesModification new;
				sourceModification: sourceModification;
				yourself ]
		ifNotNil: [ :theMethodModification | 
			modification := (RwExtensionMethodModificationForNewClassVersion
				before: theMethodModification before
				after: theMethodModification after)
				isMeta: isMeta;
				classDefinition: aMethodMove classOrExtensionAfter;
				propertiesModification: theMethodModification propertiesModification;
				sourceModification: theMethodModification sourceModification;
				yourself ].

	methodsModification at: aMethodMove methodAfter selector put: modification
]

{ #category : 'private' }
RwGsClassVersioningSymbolDictPatchV2 >> _createMethodModificationForNewVersionOfClassNamed: className methodMove: aMethodMove projectSetModification: aProjectSetModification [
	aMethodMove classOrExtensionAfter isClassExtension
		ifTrue: [ 
			self
				_createMethodExtensionModificationForNewVersionOfClassNamed: className
				isMeta: aMethodMove isMeta
				methodMove: aMethodMove
				projectSetModification: aProjectSetModification ]
		ifFalse: [ 
			self
				_createMethodAdditionModificationForNewVersionOfClassNamed: className
				isMeta: aMethodMove isMeta
				methodMove: aMethodMove
				projectSetModification: aProjectSetModification ]
]

{ #category : 'private' }
RwGsClassVersioningSymbolDictPatchV2 >> _updateNewClassVersionPatchesForClass: class in: aProjectSetModification patchSet: aPatchSet [
	| className extensionMap emptyDict |
	className := class name asString.
	extensionMap := Dictionary new.
	emptyDict := Dictionary new.
	(Rowan image loadedClassExtensionsForClass: class)
		do: [ :loadedClassExtension | 
			| dict |
			dict := extensionMap at: className ifAbsentPut: [ Dictionary new ].
			loadedClassExtension
				loadedInstanceMethodsDo: [ :loadedProject :loadedPackage :loadedClass :loadedMethod | 
					(dict at: 'instance' ifAbsentPut: [ Dictionary new ])
						at: loadedMethod selector
						put: loadedMethod ]
				loadedClassMethodsDo: [ :loadedProject :loadedPackage :loadedClass :loadedMethod | 
					(dict at: 'class' ifAbsentPut: [ Dictionary new ])
						at: loadedMethod selector
						put: loadedMethod ] ].
	extensionMap isEmpty
		ifTrue: [ ^ self ].

	aPatchSet movedMethods
		do: [ :aMethodMove | 
			| methodsDict selectorDict |
			methodsDict := extensionMap
				at: aMethodMove classOrExtensionAfter name
				ifAbsent: [ emptyDict ].

			selectorDict := aMethodMove isMeta
				ifTrue: [ methodsDict at: 'class' ifAbsent: [ emptyDict ] ]
				ifFalse: [ methodsDict at: 'instance' ifAbsent: [ emptyDict ] ].
			(selectorDict removeKey: aMethodMove methodAfter selector ifAbsent: [  ])
				ifNotNil: [ 
					"need to arrange to add the method back into the projectSetModification"
					self
						_createMethodModificationForNewVersionOfClassNamed: className
						methodMove: aMethodMove
						projectSetModification: aProjectSetModification ] ].

	aPatchSet
		allPatchesAffectingLiveMethodsDo: [ :patch | 
			"noop"
			 ]
		deletedMethodsDo: [ :patch | 
			| methodsDict selectorDict |
			methodsDict := extensionMap
				at: patch classDefinition name
				ifAbsent: [ emptyDict ].
			selectorDict := patch isMeta
				ifTrue: [ methodsDict at: 'class' ifAbsent: [ emptyDict ] ]
				ifFalse: [ methodsDict at: 'instance' ifAbsent: [ emptyDict ] ].
			selectorDict removeKey: patch methodDefinition selector ifAbsent: [  ] ].
	extensionMap
		valuesDo: [ :methodsDict | 
			(methodsDict at: 'instance' ifAbsent: [ emptyDict ])
				valuesDo: [ :loadedMethod | 
					self
						_createMethodExtensionModificationForNewVersionOfClassNamed: className
						isMeta: false
						loadedMethod: loadedMethod
						projectSetModification: aProjectSetModification ].
			(methodsDict at: 'class' ifAbsent: [ emptyDict ])
				valuesDo: [ :loadedMethod | 
					self
						_createMethodExtensionModificationForNewVersionOfClassNamed: className
						isMeta: true
						loadedMethod: loadedMethod
						projectSetModification: aProjectSetModification ] ]
]

{ #category : 'accessing' }
RwGsClassVersioningSymbolDictPatchV2 >> classModification: aClassModification [

	classModification := aClassModification.
	classDefinition := classModification after
]

{ #category : 'actions' }
RwGsClassVersioningSymbolDictPatchV2 >> createClassFor: aPatchSet inSymDict: symDictName [
	"we're using createClassFor: to apply the class modifications to the existing class
	oldClassVersion and produce a new class version"

	oldClassVersion := self oldClassVersion.	"grab the class version BEFORE creating newClassVersion ...it may be needed later"
	newClassVersion := super createClassFor: aPatchSet inSymDict: symDictName.
	symbolAssociation := self resolveName: newClassVersion name.
	^ newClassVersion
]

{ #category : 'accessing' }
RwGsClassVersioningSymbolDictPatchV2 >> existingSymbolDictionary [
	"answer the current symbol dictionary for the class ... before being (potentially) moved to a different symbolDictionary"

	| symDictName |
	symDictName := classModification before gs_symbolDictionary.
	^ Rowan image newOrExistingSymbolDictionaryNamed: symDictName
]

{ #category : 'accessing' }
RwGsClassVersioningSymbolDictPatchV2 >> existingSymbolDictionaryRegistry [

	^ self existingSymbolDictionary rowanSymbolDictionaryRegistry

]

{ #category : 'patching' }
RwGsClassVersioningSymbolDictPatchV2 >> installNewClassVersionInSystem [
	"Install the new class association in the symbolAssociation for the class.
	 Update the LoadedClass with properties for the new classversion."

	self symbolDictionaryRegistry
		addNewClassVersionToAssociation: newClassVersion
		oldClassVersion: oldClassVersion
		implementationClass: RwGsSymbolDictionaryRegistry_ImplementationV2
]

{ #category : 'patching' }
RwGsClassVersioningSymbolDictPatchV2 >> moveNewClassVersionInSystem: aClassMove [
	"Move the class association for the class.
	 Update the LoadedClass with properties for the new classversion."

	| theRegistry |
	theRegistry := (self
		symbolDictionaryFor: aClassMove packageAfter name
		projectDefinition: aClassMove projectAfter) rowanSymbolDictionaryRegistry.

	theRegistry
		addNewClassVersionToAssociation: newClassVersion
		oldClassVersion: oldClassVersion
		implementationClass: RwGsSymbolDictionaryRegistry_ImplementationV2
]

{ #category : 'versioning' }
RwGsClassVersioningSymbolDictPatchV2 >> newClassVersion [

	^ newClassVersion
]

{ #category : 'accessing' }
RwGsClassVersioningSymbolDictPatchV2 >> oldClassDefinition [

	^ classModification before
]

{ #category : 'versioning' }
RwGsClassVersioningSymbolDictPatchV2 >> oldClassVersion [

	"The old version is what is currently bound to the old class definition's name."

	oldClassVersion ifNotNil: [ ^ oldClassVersion ].
	^ (self resolveName: self oldClassDefinition key) value
]

{ #category : 'new version support' }
RwGsClassVersioningSymbolDictPatchV2 >> updateNewClassVersionPatchesForExtensionsIn: aProjectSetModification patchSet: aPatchSet [
	| className class |
	className := classDefinition name.
	class := self existingSymbolDictionary at: className asSymbol.
	self
		_updateNewClassVersionPatchesForClass: class
		in: aProjectSetModification
		patchSet: aPatchSet
]

{ #category : 'new version support' }
RwGsClassVersioningSymbolDictPatchV2 >> updateNewClassVersionPatchesForSubclassesIn: aProjectSetModification patchSet: patchSet [

	| subclassesOfClassWithNewVersions existingClass classesWithNewVersions |
	existingClass := self oldClassVersion.
	subclassesOfClassWithNewVersions := IdentitySet new.
	subclassesOfClassWithNewVersions
		addAll: ((ClassOrganizer new allSubclassesOf: existingClass) reject: [:class | (Rowan image loadedClassForClass: class ifAbsent: []) isNil ]).
	classesWithNewVersions := patchSet classesWithNewVersions
		collect: [ :patch | patch oldClassVersion ].
	subclassesOfClassWithNewVersions removeAllPresent: classesWithNewVersions.
	subclassesOfClassWithNewVersions
		do: [ :subclass | 
			| loadedClass loadedPackage loadedProject loadedClassDefinition subclassName newVersionClassModification 
				projectModification packageModification classesModification classesModified |
			subclassName := subclass name asString.
			loadedClass := Rowan image loadedClassNamed: subclassName.
			loadedClassDefinition := loadedClass asDefinition.
			loadedPackage := loadedClass loadedPackage.
			loadedProject := loadedClass loadedProject.
			newVersionClassModification := loadedClassDefinition
				compareAgainstBaseForcingNewClassVersion: loadedClassDefinition.

			projectModification := aProjectSetModification elementsModified 
				at: loadedProject name 
				ifAbsentPut: [
					| def |
					"arrange for anew project modification to be added"
					def := loadedProject asDefinition.
					(RwProjectModification before: def after: def)
						propertiesModification: RwPropertiesModification new;
						packagesModification: RwPackagesModification new;
						yourself ].
			packageModification := projectModification packagesModification elementsModified 
					at: loadedPackage name 
					ifAbsentPut: [  
						| def |
						"arrange for a new package modification to be added"
						def := loadedPackage asDefinition.
						(RwPackageModification before: def after: def)
							classesModification: RwClassesModification new;
							classExtensionsModification: RwClassExtensionsModification new;
							yourself  ].
			classesModification := packageModification classesModification.
			classesModified := classesModification elementsModified.
			classesModification isEmpty
				ifTrue: [ 
					classesModified at: subclassName put: newVersionClassModification.
					newVersionClassModification
						mergeForNewClassVersionWith: newVersionClassModification ]
				ifFalse: [ 
					(classesModified at: subclassName ifAbsent: [  ])
						ifNil: [
							classesModified at: subclassName put: newVersionClassModification.
							newVersionClassModification
								mergeForNewClassVersionWith: newVersionClassModification ]
						ifNotNil: [ :existingClassModification | 
							existingClassModification isDeletion
								ifTrue: [ 
									"a deletion is not unexpected ... "
									]
								ifFalse: [
									newVersionClassModification mergeForExistingClassWith: existingClassModification.
									classesModified at: subclassName put: newVersionClassModification ] ] ] .
			self
				_updateNewClassVersionPatchesForClass: subclass
				in: aProjectSetModification
				patchSet: patchSet ]
]

{ #category : 'new version support' }
RwGsClassVersioningSymbolDictPatchV2 >> updatePatchesForNewClassVersion: aProjectSetModification patchSetSymbolList: patchSet [
	| existingClass loadedClass loadedPackageName loadedClassDefinition newVersionClassModification existingClassName movedDeletedMap |
	movedDeletedMap := Dictionary new.
	(patchSet class
		lookupSymbolDictName: self symbolDictionaryName
		in: patchSet movedClassesSymbolList)
		keysAndValuesDo: [ :className :classMove | movedDeletedMap at: className put: classMove ].
	existingClass := self oldClassVersion.
	existingClassName := existingClass name asString.

	loadedClass := self existingSymbolDictionaryRegistry
		existingForClass: existingClass.
	loadedClassDefinition := loadedClass asDefinition.
	loadedPackageName := loadedClass loadedPackage name.

	newVersionClassModification := self classDefinition
		compareAgainstBaseForNewClassVersion: loadedClassDefinition.
	newVersionClassModification isEmpty
		ifFalse: [ 
			"only newVersionClassModification with substance need further processing"
			aProjectSetModification
				classesModificationAndPackageModificationAndProjectModificationDo: [ :classesModification :packageModification | 
					classesModification isEmpty
						ifFalse: [ 
							| classesModified |
							classesModified := classesModification elementsModified.
							(classesModified at: existingClassName ifAbsent: [  ])
								ifNil: [ 
									"not unexpected ... if there are multiple packages involved"
									 ]
								ifNotNil: [ :existingClassModification | 
									| deleteClassModification |
									deleteClassModification := false.
									(movedDeletedMap at: existingClassName ifAbsent: [  ])
										ifNotNil: [ :classMove | 
											deleteClassModification := classMove packageBefore name
												= packageModification after name ].
									deleteClassModification
										ifTrue: [ classesModified removeKey: existingClassName ]
										ifFalse: [ 
											newVersionClassModification mergeForExistingClassWith: existingClassModification.
											classesModified
												at: existingClassName
												put: newVersionClassModification ] ] ] ] ]
]
