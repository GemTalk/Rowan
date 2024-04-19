Class {
	#name : 'RwGsClassVersioningPatchV2',
	#superclass : 'RwGsClassPatchV2',
	#instVars : [
		'classModification',
		'oldClassVersion',
		'newClassVersion',
		'symbolAssociation'
	],
	#category : 'Rowan-GemStone-LoaderV2'
}

{ #category : 'patching' }
RwGsClassVersioningPatchV2 class >> addPatchedClassModification: aClassModification inPackage: aPackageDefinition inProject: aProjectDefinition toPatchSet: aRwGsPatchSet [

	"Need to decide whether the patches in aClassModification warrant a new class version or 
	 simple property changes."

	^ self new
		addPatchedClassModification: aClassModification
		inPackage: aPackageDefinition
		inProject: aProjectDefinition
		toPatchSet: aRwGsPatchSet
]

{ #category : 'patching' }
RwGsClassVersioningPatchV2 class >> addPatchedClassModificationForcingNewClassVersion: aClassModification inPackage: aPackageDefinition inProject: aProjectDefinition toPatchSet: aRwGsPatchSet [

	"Need to decide whether the patches in aClassModification warrant a new class version or 
	 simple property changes."

	^ self new
		addPatchedClassModificationForcingNewClassVersion: aClassModification
		inPackage: aPackageDefinition
		inProject: aProjectDefinition
		toPatchSet: aRwGsPatchSet
]

{ #category : 'instance creation' }
RwGsClassVersioningPatchV2 class >> for: aClassModification inPackage: aPackageDefinition [

	^ self new
		classModification: aClassModification;
		packageDefinition: aPackageDefinition;
		yourself
]

{ #category : 'private' }
RwGsClassVersioningPatchV2 >> _newSubclassWithSuperclass: newSuperclass isEquivalentToSubclass: oldClass 
newOpts: optionsArray newFormat: theFormat newInstVars: anArrayOfInstvarNames newClassInstVars: anArrayOfClassInstVars 
newPools: anArrayOfPoolDicts newClassVars: anArrayOfClassVars newConstraints: aConstraint
patchedClassProperties: patchedClassPropertiesBlock 
patchedConstraints: patchedConstraintsBlock 
patchedClassVars: patchedClassVarsBlock 
patchedClassNewVersions: patchedClassNewVersionsBlock [

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

	| supr opts ivs civs poolds cvars cons cvarsPatched |
	cvarsPatched := (anArrayOfClassVars asSortedCollection = (oldClass classVarNames collect: [:each | each asString ]) asSortedCollection) not.
	self
		_newSubclassWithSuperclass: newSuperclass
		isEquivalentToSubclass: oldClass
		newOpts: optionsArray
		newFormat: theFormat
		newInstVars: anArrayOfInstvarNames
		newClassInstVars: anArrayOfClassInstVars
		newPools: anArrayOfPoolDicts
		newClassVars: anArrayOfClassVars
		newConstraints: aConstraint
		suprBlock: [ :bool | supr := bool not ]
		optsBlock: [ :arg | "arg is true or a String" opts := arg ~~ true  ]
		ivsBlock: [ :bool | ivs := bool not ]
		civsBlock: [ :bool | civs := bool not ]
		poolsBlock: [ :bool | poolds := bool not ]
		cvarsBlock: [ :bool | cvars := bool not ]
		consBlock:  [ :bool | cons := bool not ].
	(supr or: [ opts or: [ ivs or: [ civs or: [ poolds or: [cvars ] ] ] ] ])
		ifTrue: [ 
			"new class version means that class vars will be patched (for free) before methods compiled"
			patchedClassNewVersionsBlock value.
			^ self ].
	cvarsPatched
		ifTrue: [ 
			"have to patch class variables BEFORE methods are compiled"
			patchedClassVarsBlock value ].
	cons
		ifTrue: [
			"constraint changes are added in separate step after all classes have been created"
			patchedConstraintsBlock value ].
	"class property changes that do not affect method compilation"
	patchedClassPropertiesBlock value
]

{ #category : 'private' }
RwGsClassVersioningPatchV2 >> _newSubclassWithSuperclass: newSuperclass isEquivalentToSubclass: oldClass newOpts: optionsArray newFormat: theFormat newInstVars: anArrayOfInstvarNames newClassInstVars: anArrayOfClassInstVars newPools: anArrayOfPoolDicts newClassVars: anArrayOfClassVars newConstraints: aConstraint suprBlock: suprBlock optsBlock: optsBlock ivsBlock: ivsBlock civsBlock: civsBlock poolsBlock: poolsBlock cvarsBlock: cvarsBlock consBlock: consBlock [
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
	fmtArr := newSuperclass
		ifNil: [ 
			Object
				_validateOptions: optionsArray
				withFormat: theFormat
				newClassName: oldClass name asString ]
		ifNotNil: [ 
			newSuperclass
				_validateOptions: optionsArray
				withFormat: theFormat
				newClassName: oldClass name asString ].
	(oldClass isKindOf: Class)
		ifFalse: [ oldClass _validateClass: Class ].
	suprBlock value: oldClass superClass == newSuperclass.
	optsBlock value: (oldClass _optionsChangableTo: fmtArr) == true .
	ivsBlock value: (oldClass _instVarsEqual: anArrayOfInstvarNames).
	civsBlock value: (oldClass class _instVarsEqual: anArrayOfClassInstVars).
	poolsBlock value: (oldClass _poolDictsEqual: anArrayOfPoolDicts).
	cvarsBlock value: (oldClass _classVarsChangableTo: anArrayOfClassVars copy).
	consBlock
		value: (aConstraint size = 0 or: [ oldClass _constraintsEqual: aConstraint ])
]

{ #category : 'patching' }
RwGsClassVersioningPatchV2 >> addPatchedClassModification: aClassModification inPackage: aPackageDefinition inProject: aProjectDefinition toPatchSet: aRwGsPatchSet [
	"Need to decide whether the patches in aClassModification warrant a new class version or 
	 simple property changes."

	| dict existingClass newFormat afterClassDefinition beforeClassDefinition newSuperclassAssoc afterSymDict beforeSymDict superclassFormat |
	packageDefinition := aPackageDefinition.
	projectDefinition := aProjectDefinition.
	afterClassDefinition := aClassModification after.
	beforeClassDefinition := aClassModification before.
	(newSuperclassAssoc := self resolveName: afterClassDefinition superclassName)
		ifNil: [ 
			"new superclass does not currently exist ... so it will be a new class, which means a new class version, guaranteed"
			aRwGsPatchSet
				addPatchedClassNewVersion: aClassModification
				inPackage: aPackageDefinition
				inProject: aProjectDefinition.
			^ self ].
	dict := Rowan image symbolDictNamed: beforeClassDefinition gs_symbolDictionary.
	existingClass := dict
		at: beforeClassDefinition name asSymbol
		ifAbsent: [ self error: 'Internal error. Attempt to modify a class whose name is not bound.' ].
	superclassFormat := existingClass superclass
		ifNil: [ Object format ]
		ifNotNil: [ :superClass | superClass format ].
	newFormat := self
		_classFormat: superclassFormat
		forSubclassType: afterClassDefinition classType.
	afterSymDict := self symbolDictionary name asString.
	beforeSymDict := beforeClassDefinition gs_symbolDictionary.
	beforeSymDict ~= afterSymDict
		ifTrue: [ 
			aRwGsPatchSet
				addPatchedClassSymbolDictionaryMove: aClassModification
				inPackage: aPackageDefinition
				inProject: aProjectDefinition ].
	self
		_newSubclassWithSuperclass: newSuperclassAssoc value
		isEquivalentToSubclass: existingClass
		newOpts: (afterClassDefinition gs_options collect: [ :each | each asSymbol ])
		newFormat: newFormat
		newInstVars: afterClassDefinition instVarNames
		newClassInstVars: afterClassDefinition classInstVarNames
		newPools: afterClassDefinition poolDictionaryNames
		newClassVars: afterClassDefinition classVarNames
		newConstraints:
			(self _gemStoneConstraintsFrom: afterClassDefinition gs_constraints)
		patchedClassProperties: [ 
			aClassModification propertiesModification isEmpty
				ifFalse: [ 
					aRwGsPatchSet
						addPatchedClassProperties: afterClassDefinition
						inPackage: aPackageDefinition
						inProject: aProjectDefinition ] ]
		patchedConstraints: [ 
			aRwGsPatchSet
				addPatchedClassConstraints: afterClassDefinition
				inPackage: aPackageDefinition
				inProject: aProjectDefinition ]
		patchedClassVars: [ 
			aRwGsPatchSet
				addPatchedClassClassVariables: afterClassDefinition
				inPackage: aPackageDefinition
				inProject: aProjectDefinition ]
		patchedClassNewVersions: [ 
			aRwGsPatchSet
				addPatchedClassNewVersion: aClassModification
				inPackage: aPackageDefinition
				inProject: aProjectDefinition ]
]

{ #category : 'patching' }
RwGsClassVersioningPatchV2 >> addPatchedClassModificationForcingNewClassVersion: aClassModification inPackage: aPackageDefinition inProject: aProjectDefinition toPatchSet: aRwGsPatchSet [
	"Uncontitionally create a new class version"

	| dict existingClass beforeClassDefinition afterSymDict beforeSymDict |
	packageDefinition := aPackageDefinition.
	projectDefinition := aProjectDefinition.
	beforeClassDefinition := aClassModification before.
	dict := aRwGsPatchSet loadSymbolList
		objectNamed: beforeClassDefinition gs_symbolDictionary.
	existingClass := dict
		at: beforeClassDefinition name asSymbol
		ifAbsent: [ self error: 'Internal error. Attempt to modify a class whose name is not bound.' ].
	aRwGsPatchSet
		addPatchedClassNewVersion: aClassModification
		inPackage: aPackageDefinition
		inProject: aProjectDefinition.
	afterSymDict := self symbolDictionary name asString.
	beforeSymDict := beforeClassDefinition gs_symbolDictionary.
	beforeSymDict ~= afterSymDict
		ifTrue: [ 
			aRwGsPatchSet
				addPatchedClassSymbolDictionaryMove: aClassModification
				inPackage: aPackageDefinition
				inProject: aProjectDefinition ]
]

{ #category : 'accessing' }
RwGsClassVersioningPatchV2 >> classModification: aClassModification [

	classModification := aClassModification.
	classDefinition := classModification after
]

{ #category : 'accessing' }
RwGsClassVersioningPatchV2 >> oldClassDefinition [

	^ classModification before
]

{ #category : 'versioning' }
RwGsClassVersioningPatchV2 >> oldClassVersion [

	"The old version is what is currently bound to the old class definition's name."

	^ (self resolveName: self oldClassDefinition key) value
]
