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

{ #category : 'patching' }
RwGsClassVersioningSymbolDictPatch class >> addPatchedClassModification: aClassModification inPackage: aPackageDefinition toPatchSet: aRwGsPatchSet [
	"Need to decide whether the patches in aClassModification warrant a new class version or 
	 simple property changes."

	^ (self new)
		addPatchedClassModification: aClassModification
		inPackage: aPackageDefinition 
		toPatchSet: aRwGsPatchSet

]

{ #category : 'instance creation' }
RwGsClassVersioningSymbolDictPatch class >> for: aClassModification inPackage: aPackageDefinition [

	^ self new
		classModification: aClassModification;
		packageDefinition: aPackageDefinition;
		yourself
]

{ #category : 'private' }
RwGsClassVersioningSymbolDictPatch >> _newSubclassWithSuperclass: newSuperclass isEquivalentToSubclass: oldClass newOpts: optionsArray newFormat: theFormat newInstVars: anArrayOfInstvarNames newClassInstVars: anArrayOfClassInstVars newPools: anArrayOfPoolDicts newClassVars: anArrayOfClassVars patchedClassProperties: patchedClassPropertiesBlock patchedClassVars: patchedClassVarsBlock patchedClassNewVersions: patchedClassNewVersionsBlock [

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
		suprBlock: [ :bool | supr := bool not ]
		optsBlock: [ :bool | opts := bool not ]
		ivsBlock: [ :bool | ivs := bool not ]
		civsBlock: [ :bool | civs := bool not ]
		poolsBlock: [ :bool | poolds := bool not ]
		cvarsBlock: [ :bool | cvars := bool not ].
	(supr or: [ opts or: [ ivs or: [ civs or: [ poolds ] ] ] ])
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
RwGsClassVersioningSymbolDictPatch >> _newSubclassWithSuperclass: newSuperclass isEquivalentToSubclass: oldClass newOpts: optionsArray newFormat: theFormat newInstVars: anArrayOfInstvarNames newClassInstVars: anArrayOfClassInstVars newPools: anArrayOfPoolDicts newClassVars: anArrayOfClassVars suprBlock: suprBlock optsBlock: optsBlock ivsBlock: ivsBlock civsBlock: civsBlock poolsBlock: poolsBlock cvarsBlock: cvarsBlock [

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
RwGsClassVersioningSymbolDictPatch >> addPatchedClassModification: aClassModification inPackage: aPackageDefinition toPatchSet: aRwGsPatchSet [

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

{ #category : 'accessing' }
RwGsClassVersioningSymbolDictPatch >> classModification: aClassModification [

	classModification := aClassModification.
	classDefinition := classModification after
]

{ #category : 'actions' }
RwGsClassVersioningSymbolDictPatch >> createClassFor: aPatchSet [
	"we're using createClassFor: to apply the class modifications to the existing class
	oldClassVersion and produce a new class version"

	oldClassVersion := self oldClassVersion.
	newClassVersion := super createClassFor: aPatchSet.
	symbolAssociation := self resolveName: newClassVersion name.
	^ newClassVersion
]

{ #category : 'patching' }
RwGsClassVersioningSymbolDictPatch >> installNewClassVerionInSystem [

	"Install the new class association in the symbolAssociation for the class.
	 Update the LoadedClass with properties for the new classversion."

	packageSymbolDictionary addNewClassVersionToAssociation: newClassVersion
]

{ #category : 'new version support' }
RwGsClassVersioningSymbolDictPatch >> newClassVersionPatches: aPackageSetModification patchSet: newClassVersionPatchSet [

	| existingClass loadedClassDefinition newVersionClassModification existingClassName |
	existingClass := self oldClassVersion.
	existingClassName := existingClass name asString.
	loadedClassDefinition := (packageSymbolDictionary
		existingForClass: existingClass) asDefinition.
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
RwGsClassVersioningSymbolDictPatch >> oldClassDefinition [

	^ classModification before
]

{ #category : 'versioning' }
RwGsClassVersioningSymbolDictPatch >> oldClassVersion [

	"The old version is what is currently bound to the old class definition's name."

	^ (self resolveName: self oldClassDefinition key) value
]
