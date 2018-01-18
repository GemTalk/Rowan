"
No class-specific documentation for CypGsClassVersioningPatch, hierarchy is: 
Object
  CypGsPatch
    CypGsClassVersioningPatch( oldClassDefinition newClassDefinition)

"
Class {
	#name : 'RwGsClassVersioningPatch',
	#superclass : 'RwGsClassPatch',
	#instVars : [
		'classModification',
		'oldClassVersion',
		'newClassVersion',
		'symbolAssociation'
	],
	#category : 'Rowan-GemStone'
}

{ #category : 'patching' }
RwGsClassVersioningPatch class >> addPatchedClassModification: aClassModification inPackage: aPackageDefinition toPatchSet: aRwGsPatchSet [
	"Need to decide whether the patches in aClassModification warrant a new class version or 
	 simple property changes."

	^ (self new)
		addPatchedClassModification: aClassModification
		inPackage: aPackageDefinition 
		toPatchSet: aRwGsPatchSet

]

{ #category : 'instance creation' }
RwGsClassVersioningPatch class >> for: aClassModification inPackage: aPackageDefinition [

	^ self new
		classModification: aClassModification;
		packageDefinition: aPackageDefinition;
		yourself
]

{ #category : 'private' }
RwGsClassVersioningPatch >> _newSubclassWithSuperclass: newSuperclass isEquivalentToSubclass: oldClass newOpts: optionsArray newFormat: theFormat newInstVars: anArrayOfInstvarNames newClassInstVars: anArrayOfClassInstVars newPools: anArrayOfPoolDicts newClassVars: anArrayOfClassVars patchedClassProperties: patchedClassPropertiesBlock patchedClassVars: patchedClassVarsBlock patchedClassNewVersions: patchedClassNewVersionsBlock [

	" based on Class>>_equivalentSubclass:superCls:name:newOpts:newFormat:newInstVars:newClassInstVars:newPools:newClassVars:inDict:isKernel: and ultimately needs to be rolled back into base, so that class creation and Rowan use the same new class version rules.
"

	"Class>>_equivalentSubclass:... has unwanted side effects"

	"squeezed down to the bare minimum"

	"oldClass is equivalent to the subclass that would be created using
 the other arguments if 
     instVar names match exactly ,
   and class instVar names match exactly ,
   and the classVars in oldClass can be modified to add/remove Associations 
     to match anArrayOfClassVars ,
   and pool dictionaries match exactly

  With respect to options and format, oldClass is equivalent if
    The state of format bits dbTransient, instancesNonPersistent, instancesInvariant  
     match exactly ,
    and subclassesDisallowed cannot be set in the new subclass if it not set in oldClass ,
    and modifiable  cannot be set if it is not set in oldClass  ,
    and  (SELF_CAN_BE_SPECIAL, NSC_DUPLICATES, INDEXABLE, IMPLEMENTATION, NO_STRUCT_UPDATE bits)
        of the formats must match exactly.
"

	| supr opts ivs civs poolds cvars ans cvarsOnly cvarsPatched |
	cvarsPatched := anArrayOfClassVars isEmpty not.
	self
		_newSubclassWithSuperclass: newSuperclass
		isEquivalentToSubclass: oldClass
		newOpts: optionsArray
		newFormat: theFormat
		newInstVars: anArrayOfInstvarNames
		newClassInstVars: anArrayOfClassInstVars
		newPools: anArrayOfPoolDicts
		newClassVars: anArrayOfClassVars
		suprBlock: [ :bool | supr := bool ]
		optsBlock: [ :bool | opts := bool ]
		ivsBlock: [ :bool | ivs := bool ]
		civsBlock: [ :bool | civs := bool ]
		poolsBlock: [ :bool | poolds := bool ]
		cvarsBlock: [ :bool | cvars := bool ].
	(supr or: [ opts or: [ ivs or: [ civs or: [ poolds or: [ cvars ] ] ] ] ])
		ifTrue: [ 
			"new class version means that class vars will be patched (for free) before methods compiled"
			patchedClassNewVersionsBlock value.
			^ self ].
	cvarsPatched
		ifTrue: [ 
			"have to patch class variables BEFORE methods are compiled"
			patchedClassVarsBlock value ]
		ifFalse: [ 
			"class property changes that do not affect method compilation"
			patchedClassPropertiesBlock value ]
]

{ #category : 'private' }
RwGsClassVersioningPatch >> _newSubclassWithSuperclass: newSuperclass isEquivalentToSubclass: oldClass newOpts: optionsArray newFormat: theFormat newInstVars: anArrayOfInstvarNames newClassInstVars: anArrayOfClassInstVars newPools: anArrayOfPoolDicts newClassVars: anArrayOfClassVars suprBlock: suprBlock optsBlock: optsBlock ivsBlock: ivsBlock civsBlock: civsBlock poolsBlock: poolsBlock cvarsBlock: cvarsBlock [

	" based on Class>>_equivalentSubclass:superCls:name:newOpts:newFormat:newInstVars:newClassInstVars:newPools:newClassVars:inDict:isKernel: and ultimately needs to be rolled back into base, so that class creation and Rowan use the same new class version rules.
"

	"Class>>_equivalentSubclass:... has unwanted side effects"

	"squeezed down to the bare minimum"

	"oldClass is equivalent to the subclass that would be created using
 the other arguments if 
     instVar names match exactly ,
   and class instVar names match exactly ,
   and the classVars in oldClass can be modified to add/remove Associations 
     to match anArrayOfClassVars ,
   and pool dictionaries match exactly

  With respect to options and format, oldClass is equivalent if
    The state of format bits dbTransient, instancesNonPersistent, instancesInvariant  
     match exactly ,
    and subclassesDisallowed cannot be set in the new subclass if it not set in oldClass ,
    and modifiable  cannot be set if it is not set in oldClass  ,
    and  (SELF_CAN_BE_SPECIAL, NSC_DUPLICATES, INDEXABLE, IMPLEMENTATION, NO_STRUCT_UPDATE bits)
        of the formats must match exactly.
"

	| fmtArr |
	fmtArr := newSuperclass _validateOptions: optionsArray withFormat: theFormat.
	(oldClass isKindOf: Class)
		ifFalse: [ oldClass _validateClass: Class ].
	suprBlock value: oldClass superClass == newSuperclass.
	optsBlock value: (oldClass _optionsChangableTo: fmtArr).
	ivsBlock value: (oldClass _instVarsEqual: anArrayOfInstvarNames).
	civsBlock value: (oldClass class _instVarsEqual: anArrayOfClassInstVars).
	poolsBlock value: (oldClass _poolDictsEqual: anArrayOfPoolDicts).
	cvarsBlock value: (oldClass _classVarsChangableTo: anArrayOfClassVars copy)
]

{ #category : 'patching' }
RwGsClassVersioningPatch >> addPatchedClassModification: aClassModification inPackage: aPackageDefinition toPatchSet: aRwGsPatchSet [

	"Need to decide whether the patches in aClassModification warrant a new class version or 
	 simple property changes."

	| dictName dict existingClass newFormat aClassDefinition newSuperclass |
	aClassDefinition := aClassModification after.
	dictName := aClassDefinition
		propertyAt: 'gs_SymbolDictionary'
		ifAbsent: [ 'UserGlobals' ].
	dict := self existingSymbolDictionaryNamed: dictName.
	dict
		ifNil: [ 
			self
				error:
					'internal error - symbolDictionary named: ' , dictName printString
						, ' not found' ].
	existingClass := dict
		at: aClassDefinition name asSymbol
		ifAbsent: [ self error: 'Internal error. Attempt to modify a class whose name is not bound.' ].
	newFormat := self
		_classFormat: existingClass superclass format
		forSubclassType: aClassDefinition classType.
	newSuperclass := (self resolveName: aClassDefinition superclassName) value.
	self
		_newSubclassWithSuperclass: newSuperclass
		isEquivalentToSubclass: existingClass
		newOpts: (aClassDefinition propertyAt: 'gs_options' ifAbsent: [ #() ])
		newFormat: newFormat
		newInstVars: aClassDefinition instVarNames
		newClassInstVars: aClassDefinition classInstVarNames
		newPools: aClassDefinition poolDictionaryNames
		newClassVars: aClassDefinition classVarNames
		patchedClassProperties: [ 
			aRwGsPatchSet
				addPatchedClassProperties: aClassDefinition
				inPackage: aPackageDefinition ]
		patchedClassVars: [ 
			aRwGsPatchSet
				addPatchedClassClassVariables: aClassDefinition
				inPackage: aPackageDefinition ]
		patchedClassNewVersions: [ 
			aRwGsPatchSet
				addPatchedClassNewVersion: aClassModification
				inPackage: aPackageDefinition ]
]

{ #category : 'actions' }
RwGsClassVersioningPatch >> addToNewClassesByName: aDictionary [

	"Dictionary is class name -> classAdditionPatch. Error on duplicate name."

	| name |
	name := self classDefinition key.
	name ifNil: [ self error: 'Class definition with no name.' ].
	(aDictionary includesKey: name)
		ifTrue: [ self error: 'Duplicate name' ].
	aDictionary at: name put: self
]

{ #category : 'accessing' }
RwGsClassVersioningPatch >> classModification: aClassModification [

	classModification := aClassModification.
	classDefinition := classModification after
]

{ #category : 'actions' }
RwGsClassVersioningPatch >> createClassFor: aPatchSet [
	"we're using createClassFor: to apply the class modifications to the existing class
	oldClassVersion and produce a new class version"

	oldClassVersion := self oldClassVersion.
	newClassVersion := super createClassFor: aPatchSet.
	symbolAssociation := self resolveName: newClassVersion name.
	^ newClassVersion
]

{ #category : 'patching' }
RwGsClassVersioningPatch >> installNewClassVerionInSystem [
	"Install the new class association in the symbolAssociation for the class.
	 Update the LoadedClass with properties for the new classversion."

	self
		updateSymbolAssociation;
		updateLoadedClass
]

{ #category : 'new version support' }
RwGsClassVersioningPatch >> newClassVersionPatches: aPackageSetModification patchSet: newClassVersionPatchSet [

	| existingClass loadedClassDefinition newVersionClassModification existingClassName |
	existingClass := self oldClassVersion.
	existingClassName := existingClass name asString.
	loadedClassDefinition := (RwGsLoadedClass existingForClass: existingClass)
		asDefinition.
	newVersionClassModification := self classDefinition
		compareAgainstBaseForNewClassVersion: loadedClassDefinition.
	newVersionClassModification isEmpty
		ifFalse: [ 
			"only newVersionClassModification with substance need further processing"
			aPackageSetModification elementsModified
				do: [ :packageModification | 
					| classesModified |
					classesModified := packageModification classesModification elementsModified.
					(classesModified at: existingClassName ifAbsent: [  ])
						ifNil: [ 
							self
								error:
									'internal error - missing class modification for '
										, existingClassName printString ]
						ifNotNil: [ :existingClassModification | 
							existingClassModification
								mergeForNewClassVersionWith: newVersionClassModification ] ] ]
]

{ #category : 'accessing' }
RwGsClassVersioningPatch >> oldClassDefinition [

	^ classModification before
]

{ #category : 'versioning' }
RwGsClassVersioningPatch >> oldClassVersion [

	"The old version is what is currently bound to the old class definition's name."

	^ (self resolveName: self oldClassDefinition key) value
]

{ #category : 'actions' }
RwGsClassVersioningPatch >> updateLoadedClass [
	"Refresh the class properties if a new class version was created."

	| loadedClass |
	oldClassVersion == newClassVersion
		ifTrue: [ ^ self ].
	loadedClass := RwGsLoadedClass existingForClass: oldClassVersion.
	loadedClass handleClassDeletionOrNewVersion
		ifTrue: [ 
			loadedClass
				updatePropertiesFromClass;
				updateForCategoryChange ]
		ifFalse: [ 
			self error: 'internal error - new version of class not properly installed'
			 ]
]

{ #category : 'actions' }
RwGsClassVersioningPatch >> updateSymbolAssociation [

	oldClassVersion == newClassVersion 
		ifTrue: [ 
			"no newClassVersion created when property modifications applied .. not a problem"
			^self ].
	symbolAssociation value: newClassVersion
]
