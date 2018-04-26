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
	#category : 'Rowan-GemStone'
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

	| loadedProject loadedPackage loadedClassExtension projectModification packageModification classExtensionModification methodsModification methodDef source sourceModification modification |
	loadedClassExtension := loadedMethod loadedClass.
	loadedPackage := loadedMethod loadedPackage.
	loadedProject := loadedMethod loadedProject.
	projectModification := aProjectSetModification elementsModified
		at: loadedProject name.
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
		ifNotNil: [ 
			"should have been filtered out earlier"
			self
				error:
					'Unexpected method modification for ' , loadedMethod selector printString ].

	methodDef := loadedMethod asDefinition.
	source := methodDef source.
	sourceModification := RwSourceModification new
		addElementModification:
				(RwUnconditionalPropertyModification
						key: 'source'
						oldValue: source
						newValue: source);
		yourself.
	modification := (RwExtensionMethodModificationForNewClassVersion
		before: methodDef
		after: methodDef)
		isMeta: isMeta;
		classDefinition: loadedClassExtension asDefinition;
		propertiesModification: RwPropertiesModification new;
		sourceModification: sourceModification;
		yourself.
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
				ifFalse: [ methodsDict at: 'instance' ifAbsent: [ emptyDict ] ].
			(selectorDict includesKey: patch methodDefinition selector)
				ifTrue: [ self error: 'not yet implemented' ] ]
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

{ #category : 'patching' }
RwGsClassVersioningSymbolDictPatch >> installNewClassVerionInSystem [

	"Install the new class association in the symbolAssociation for the class.
	 Update the LoadedClass with properties for the new classversion."

	self symbolDictionaryRegistry addNewClassVersionToAssociation: newClassVersion
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
		addAll: (ClassOrganizer new allSubclassesOf: existingClass).
	classesWithNewVersions := patchSet classesWithNewVersions
		collect: [ :patch | patch oldClassVersion ].
	subclassesOfClassWithNewVersions removeAllPresent: classesWithNewVersions.
	subclassesOfClassWithNewVersions
		do: [ :subclass | 
			| loadedClassDefinition subclassName newVersionClassModification |
			subclassName := subclass name asString.
			loadedClassDefinition := (self symbolDictionaryRegistry
				existingForClass: subclass) asDefinition.
			newVersionClassModification := loadedClassDefinition
				compareAgainstBaseForcingNewClassVersion: loadedClassDefinition.
			aProjectSetModification
				classesModifiedDo: [ :classesModification | 
					classesModification isEmpty
						ifTrue: [ 
							"this is not unexpected ... not all packages in the project set are expected to have class modifications with new versions"
							 ]
						ifFalse: [ 
							| classesModified |
							classesModified := classesModification elementsModified.
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
											existingClassModification
												mergeForNewClassVersionWith: newVersionClassModification ] ] ] ].
			self
				_updateNewClassVersionPatchesForClass: subclass
				in: aProjectSetModification
				patchSet: patchSet ]

]

{ #category : 'new version support' }
RwGsClassVersioningSymbolDictPatch >> updatePatchesForNewClassVersion: aProjectSetModification [

	| existingClass loadedClassDefinition newVersionClassModification existingClassName |
	existingClass := self oldClassVersion.
	existingClassName := existingClass name asString.
	loadedClassDefinition := (self symbolDictionaryRegistry
		existingForClass: existingClass) asDefinition.
	newVersionClassModification := self classDefinition
		compareAgainstBaseForNewClassVersion: loadedClassDefinition.
	newVersionClassModification isEmpty
		ifFalse: [ 
			"only newVersionClassModification with substance need further processing"
			aProjectSetModification
				classesModifiedDo: [ :classesModification | 
					classesModification isEmpty
						ifFalse: [ 
							| classesModified |
							classesModified := classesModification elementsModified.
							(classesModified at: existingClassName ifAbsent: [  ])
								ifNil: [ "not unexpected ... if there are multiple packages involved" ]
								ifNotNil: [ :existingClassModification | 
									existingClassModification
										mergeForNewClassVersionWith: newVersionClassModification ] ] ] ]

]
