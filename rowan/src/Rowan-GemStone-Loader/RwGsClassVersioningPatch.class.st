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
	#category : 'Rowan-GemStone-Loader'
}

{ #category : 'patching' }
RwGsClassVersioningPatch class >> addPatchedClassModification: aClassModification inPackage: aPackageDefinition inProject: aProjectDefinition toPatchSet: aRwGsPatchSet [

	"Need to decide whether the patches in aClassModification warrant a new class version or 
	 simple property changes."

	^ self new
		addPatchedClassModification: aClassModification
		inPackage: aPackageDefinition
		inProject: aProjectDefinition
		toPatchSet: aRwGsPatchSet
]

{ #category : 'patching' }
RwGsClassVersioningPatch class >> addPatchedClassModificationForcingNewClassVersion: aClassModification inPackage: aPackageDefinition inProject: aProjectDefinition toPatchSet: aRwGsPatchSet [

	"Need to decide whether the patches in aClassModification warrant a new class version or 
	 simple property changes."

	^ self new
		addPatchedClassModificationForcingNewClassVersion: aClassModification
		inPackage: aPackageDefinition
		inProject: aProjectDefinition
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
RwGsClassVersioningPatch >> _newSubclassWithSuperclass: newSuperclass isEquivalentToSubclass: oldClass 
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
	cvarsPatched := (anArrayOfClassVars asSortedCollection = oldClass classVarNames asSortedCollection) not.
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
		optsBlock: [ :bool | opts := bool not ]
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

{ #category : 'patching' }
RwGsClassVersioningPatch >> addPatchedClassModification: aClassModification inPackage: aPackageDefinition inProject: aProjectDefinition toPatchSet: aRwGsPatchSet [

	"Need to decide whether the patches in aClassModification warrant a new class version or 
	 simple property changes."

	| dict existingClass newFormat afterClassDefinition beforeClassDefinition newSuperclassAssoc afterSymDict beforeSymDict |
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
	dict := Rowan image symbolList objectNamed: beforeClassDefinition gs_symbolDictionary.
	existingClass := dict
		at: beforeClassDefinition name asSymbol
		ifAbsent: [ self error: 'Internal error. Attempt to modify a class whose name is not bound.' ].
	newFormat := self
		_classFormat: existingClass superclass format
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
		newOpts: (afterClassDefinition gs_options collect: [:each | each asSymbol])
		newFormat: newFormat
		newInstVars: afterClassDefinition instVarNames
		newClassInstVars: afterClassDefinition classInstVarNames
		newPools: afterClassDefinition poolDictionaryNames
		newClassVars: afterClassDefinition classVarNames
		newConstraints: (self _gemStoneConstraintsFrom: afterClassDefinition gs_constraints)
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
RwGsClassVersioningPatch >> addPatchedClassModificationForcingNewClassVersion: aClassModification inPackage: aPackageDefinition inProject: aProjectDefinition toPatchSet: aRwGsPatchSet [

	"Uncontitionally create a new class version"

	| dict existingClass beforeClassDefinition |
	packageDefinition := aPackageDefinition.
	projectDefinition := aProjectDefinition.
	beforeClassDefinition := aClassModification before.
	dict := Rowan image symbolList objectNamed: beforeClassDefinition gs_symbolDictionary.
	existingClass := dict
		at: beforeClassDefinition name asSymbol
		ifAbsent: [ self error: 'Internal error. Attempt to modify a class whose name is not bound.' ].
	aRwGsPatchSet
		addPatchedClassNewVersion: aClassModification
		inPackage: aPackageDefinition
		inProject: aProjectDefinition

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
RwGsClassVersioningPatch >> updateSymbolAssociation [

	oldClassVersion == newClassVersion 
		ifTrue: [ 
			"no newClassVersion created when property modifications applied .. not a problem"
			^self ].
	symbolAssociation value: newClassVersion
]
