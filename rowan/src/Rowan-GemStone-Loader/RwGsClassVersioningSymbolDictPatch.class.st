"
No class-specific documentation for CypGsClassVersioningPatch, hierarchy is: 
Object
  CypGsPatch
    CypGsClassVersioningPatch( oldClassDefinition newClassDefinition)

"
Class {
	#name : 'RwGsClassVersioningSymbolDictPatch',
	#superclass : 'RwGsClassPatch',
	#instVars : [
		'classModification',
		'oldClassVersion',
		'newClassVersion',
		'symbolAssociation'
	],
	#category : 'Rowan-GemStone-Loader'
}

{ #category : 'instance creation' }
RwGsClassVersioningSymbolDictPatch class >> for: aClassModification inPackage: aPackageDefinition [

	^ self new
		classModification: aClassModification;
		packageDefinition: aPackageDefinition;
		yourself
]

{ #category : 'private' }
RwGsClassVersioningSymbolDictPatch >> _createMethodExtensionModificationForNewVersionOfClassNamed: className isMeta: isMeta loadedMethod: loadedMethod projectSetModification: aProjectSetModification [

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
RwGsClassVersioningSymbolDictPatch >> _updateNewClassVersionPatchesForClass: class in: aProjectSetModification patchSet: aPatchSet [

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
	aPatchSet
		allPatchesAffectingLiveMethodsDo: [ :patch | 
			| methodsDict selectorDict |
			methodsDict := extensionMap
				at: patch classDefinition name
				ifAbsent: [ emptyDict ].
			selectorDict := patch isMeta
				ifTrue: [ methodsDict at: 'class' ifAbsent: [ emptyDict ] ]
				ifFalse: [ methodsDict at: 'instance' ifAbsent: [ emptyDict ] ] ]
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
RwGsClassVersioningSymbolDictPatch >> classModification: aClassModification [

	classModification := aClassModification.
	classDefinition := classModification after
]

{ #category : 'actions' }
RwGsClassVersioningSymbolDictPatch >> createClassFor: aPatchSet [

	"we're using createClassFor: to apply the class modifications to the existing class
	oldClassVersion and produce a new class version"

	oldClassVersion := self oldClassVersion.	"grab the class version BEFORE creating newClassVersion ...it may be needed later"
	newClassVersion := super createClassFor: aPatchSet.
	symbolAssociation := self resolveName: newClassVersion name.
	^ newClassVersion
]

{ #category : 'accessing' }
RwGsClassVersioningSymbolDictPatch >> existingSymbolDictionary [
	"answer the current symbol dictionary for the class ... before being (potentially) moved to a different symbolDictionary"

	| symDictName symDict |
	symDictName := classModification before gs_symbolDictionary.
	symDict := GsCurrentSession currentSession symbolList objectNamed: symDictName asSymbol.
	symDict
		ifNotNil: [ symDict rowanSymbolDictionaryRegistry ifNotNil: [ ^ symDict ] ].
	^ Rowan image newOrExistingSymbolDictionaryNamed: symDictName

]

{ #category : 'accessing' }
RwGsClassVersioningSymbolDictPatch >> existingSymbolDictionaryRegistry [

	^ self existingSymbolDictionary rowanSymbolDictionaryRegistry

]

{ #category : 'patching' }
RwGsClassVersioningSymbolDictPatch >> installNewClassVersionInSystem [

	"Install the new class association in the symbolAssociation for the class.
	 Update the LoadedClass with properties for the new classversion."

	self symbolDictionaryRegistry addNewClassVersionToAssociation: newClassVersion implementationClass: RwGsSymbolDictionaryRegistry_Implementation

]

{ #category : 'patching' }
RwGsClassVersioningSymbolDictPatch >> moveNewClassVersionInSystem: aClassMove [

	"Move the class association for the class.
	 Update the LoadedClass with properties for the new classversion."

	(self symbolDictionaryFor: aClassMove packageAfter name projectDefinition: aClassMove projectAfter)
		rowanSymbolDictionaryRegistry
			addNewClassVersionToAssociation: newClassVersion 
				implementationClass: RwGsSymbolDictionaryRegistry_Implementation
]

{ #category : 'versioning' }
RwGsClassVersioningSymbolDictPatch >> newClassVersion [

	^ newClassVersion
]

{ #category : 'accessing' }
RwGsClassVersioningSymbolDictPatch >> oldClassDefinition [

	^ classModification before
]

{ #category : 'versioning' }
RwGsClassVersioningSymbolDictPatch >> oldClassVersion [

	"The old version is what is currently bound to the old class definition's name."

	oldClassVersion ifNotNil: [ ^ oldClassVersion ].
	^ (self resolveName: self oldClassDefinition key) value
]

{ #category : 'new version support' }
RwGsClassVersioningSymbolDictPatch >> updateNewClassVersionPatchesForExtensionsIn: aProjectSetModification patchSet: aPatchSet [

	| className class |
	className := classDefinition name.
	class := Rowan globalNamed: className.
	self
		_updateNewClassVersionPatchesForClass: class
		in: aProjectSetModification
		patchSet: aPatchSet
]

{ #category : 'new version support' }
RwGsClassVersioningSymbolDictPatch >> updateNewClassVersionPatchesForSubclassesIn: aProjectSetModification patchSet: patchSet [

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
			loadedClass := self existingSymbolDictionaryRegistry existingForClass: subclass.
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
RwGsClassVersioningSymbolDictPatch >> updatePatchesForNewClassVersion: aProjectSetModification patchSet: patchSet [

	| existingClass loadedClass loadedPackageName loadedClassDefinition newVersionClassModification existingClassName movedDeletedMap |
	movedDeletedMap := Dictionary new.
	patchSet movedClassesMap keysAndValuesDo: [:className :classMove | movedDeletedMap at: className put: classMove ].
	existingClass := self oldClassVersion.
	existingClassName := existingClass name asString.

	loadedClass := self existingSymbolDictionaryRegistry existingForClass: existingClass.
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
								ifNil: [ "not unexpected ... if there are multiple packages involved" ]
								ifNotNil: [ :existingClassModification | 
									| deleteClassModification |
									deleteClassModification := false.
									(movedDeletedMap at: existingClassName ifAbsent: []) ifNotNil: [:classMove |
										deleteClassModification := classMove packageBefore name = packageModification after name ].
									deleteClassModification
										ifTrue: [classesModified removeKey: existingClassName]
										ifFalse: [
											newVersionClassModification mergeForExistingClassWith: existingClassModification.
											classesModified at: existingClassName put: newVersionClassModification ] ] ] ] ]

]
