"
The class variable ClassRegistry holds a dictionary that maps ClassHistory -> LoadedClass for all non-meta classes that are known to be defined in a package.
"
Class {
	#name : 'CypGsLoadedClass',
	#superclass : 'CypLoadedClass',
	#classVars : [
		'ClassRegistry'
	],
	#category : 'Cypress-GemStone'
}

{ #category : 'enumeration' }
CypGsLoadedClass class >> allClassesDo: aBlock [
	"Iterate over all classes visible in the current session's transient symbol list, evaluating the block with each."

	GsCurrentSession currentSession symbolList do: 
			[:dict |
			dict do: 
					[:maybeClass |
					(maybeClass isBehavior and: [maybeClass isMeta not])
						ifTrue: [aBlock value: maybeClass]]]
]

{ #category : 'enumeration' }
CypGsLoadedClass class >> allDoSafely: aBlock [
	"Evaluate aBlock for every registered LoadedClass in the system. 
	The block may modify the ClassRegistry, so we must copy the values first."

	ClassRegistry values do: aBlock
]

{ #category : 'queries' }
CypGsLoadedClass class >> existingForClass: aClass [
	"Answer the registered LoadedClass for the given class."

	^ClassRegistry at: aClass classHistory ifAbsent: [AbsentToken]
]

{ #category : 'queries' }
CypGsLoadedClass class >> forClass: aClass [
	"Answer the registered LoadedClass for the given class. If none registered, create one, register it, and answer it."

	^ClassRegistry at: aClass classHistory ifAbsent: [self newForClass: aClass]
]

{ #category : 'class initialization' }
CypGsLoadedClass class >> initialize [
	"CypGsLoadedClass initialize"

	ClassRegistry := KeyValueDictionary new
]

{ #category : 'instance creation' }
CypGsLoadedClass class >> newForClass: aClass [

	^self new initializeForClass: aClass
]

{ #category : 'private' }
CypGsLoadedClass class >> resolveClassNamed: aSymbol [
	"If the given symbol resolves in this session, *and* if what it resolves to is a class, answer that class.
	Otherwise, answer nil."

	| possibleClass |
	possibleClass := GsCurrentSession currentSession objectNamed: aSymbol.
	^(possibleClass isBehavior and: [possibleClass isMeta not])
		ifTrue: [possibleClass]
		ifFalse: [nil]
]

{ #category : 'queries' }
CypGsLoadedClass class >> symbolDictsForClass: aClass [
	"Answer a sequenceable collection of SymbolDictionaries, among those in the current session's transient
	symbol list, wherein the given class is present by its own name."

	| className |
	className := aClass name.
	^GsCurrentSession currentSession symbolList
		select: [:dict | (dict at: className ifAbsent: [nil]) == aClass]
]

{ #category : 'methods' }
CypGsLoadedClass >> addLoadedClassMethod: aLoadedMethod [

	self markPackageDirty.
	loadedClassMethods at: aLoadedMethod key put: aLoadedMethod.
	aLoadedMethod loadedClass: self
]

{ #category : 'methods' }
CypGsLoadedClass >> addLoadedInstanceMethod: aLoadedMethod [

	self markPackageDirty.
	loadedInstanceMethods at: aLoadedMethod key put: aLoadedMethod.
	aLoadedMethod loadedClass: self
]

{ #category : 'methods' }
CypGsLoadedClass >> addLoadedMethod: aLoadedMethod [

	aLoadedMethod classIsMeta
		ifTrue: [self addLoadedClassMethod: aLoadedMethod]
		ifFalse: [self addLoadedInstanceMethod: aLoadedMethod]
]

{ #category : 'private-updating' }
CypGsLoadedClass >> handleClassDeletion [
	"The class to which I refer has been deleted, so I must unregister myself.
	Normally, I will have a loadedPackage, but if something interrupted my initialization it might be nil."

	loadedPackage ifNotNil: [loadedPackage removeLoadedClass: self].
	ClassRegistry removeKey: handle classHistory
]

{ #category : 'private-updating' }
CypGsLoadedClass >> handleClassDeletionOrNewVersion [
	"The class is deleted if both:
	* its name no longer resolves to a class in the classHistory of the class I remember.
	* no class in the classHistory of the class I remember is accessible under its name.
	If my name no longer resolves to my class, but another class in its classHistory does resolve,
	consider it a class re-version or rename.
	Answer true if the class still exists, false if it has been deleted."

	| resolved thoseResolving |
	resolved := self resolveClassNamed: name.
	resolved == handle ifTrue: [^true].

	"Has been deleted, renamed, or a new version under the same name.
	Answer false if deleted, set handle and answer true otherwise."
	thoseResolving := handle classHistory reverse
				select: [:aClass | aClass == (self resolveClassNamed: aClass name)].
	thoseResolving size = 0
		ifTrue: 
			[self handleClassDeletion.
			^false].
	thoseResolving size > 1
		ifTrue: 
			[CypNotification
				signal: 'Class history with multiple "current" members found. One associated name is '
						, name].
	handle := thoseResolving first.
	^true
]

{ #category : 'initialization' }
CypGsLoadedClass >> initialize [

	super initialize.
	loadedInstanceMethods := KeyValueDictionary new.
	loadedClassMethods := KeyValueDictionary new
]

{ #category : 'initialization' }
CypGsLoadedClass >> initializeForClass: aClass [

	self initialize.
	self initializeForName: aClass name asString.
	handle := aClass.
	ClassRegistry at: aClass classHistory put: self
]

{ #category : 'initialization' }
CypGsLoadedClass >> initializeForName: aClassName [

	super initializeForName: aClassName.
	properties at: 'name' put: aClassName
]

{ #category : 'accessing' }
CypGsLoadedClass >> key [
	"Answer some token that will uniquely identify me relative to any other LoadedClass in the same package."

	^name
]

{ #category : 'methods' }
CypGsLoadedClass >> removeLoadedClassMethod: aLoadedMethod [

	self markPackageDirty.
	loadedClassMethods removeKey: aLoadedMethod key
		ifAbsent: [self error: 'Method not present in loadedClass']
]

{ #category : 'methods' }
CypGsLoadedClass >> removeLoadedInstanceMethod: aLoadedMethod [

	self markPackageDirty.
	loadedInstanceMethods removeKey: aLoadedMethod key
		ifAbsent: [self error: 'Method not present in loadedClass']
]

{ #category : 'methods' }
CypGsLoadedClass >> removeLoadedMethod: aLoadedMethod [

	aLoadedMethod classIsMeta
		ifTrue: [self removeLoadedClassMethod: aLoadedMethod]
		ifFalse: [self removeLoadedInstanceMethod: aLoadedMethod]
]

{ #category : 'private' }
CypGsLoadedClass >> resolveClassNamed: aName [
	"If the given name is bound to a class in the environment of the current session, 
	answer that class. Otherwise, answer nil."

	| resolved |
	resolved := GsCurrentSession currentSession objectNamed: aName.
	^(resolved isBehavior and: [resolved isMeta not])
		ifTrue: [resolved]
		ifFalse: [nil]
]

{ #category : 'private' }
CypGsLoadedClass >> symbolDictionaryName: aName [

	self propertyAt: 'gs_SymbolDictionary' put: aName asString
]

{ #category : 'private-updating' }
CypGsLoadedClass >> updateCategoryFromClass [

	| propertyName oldValue newValue absentToken |
	propertyName := 'category'.
	absentToken := self absentToken.
	oldValue := self propertyAt: propertyName.
	newValue := handle category ifNil: [absentToken].
	(newValue ~~ absentToken and: [newValue isEmpty])
		ifTrue: [newValue := absentToken].

	"Absent token must be compared by identity, because a comment of '<absent>' would be equal to no comment."
	(oldValue == absentToken | (newValue == absentToken)
		ifTrue: [oldValue == newValue]
		ifFalse: [oldValue = newValue])
			ifFalse: 
				[newValue == absentToken
					ifTrue: [self removeProperty: propertyName]
					ifFalse: [self propertyAt: propertyName put: newValue]]
]

{ #category : 'private-updating' }
CypGsLoadedClass >> updateClassInstvarNamesFromClass [
	"Order of instvars does not matter to Cypress (at least not in GemStone) so we keep the instvar names sorted."

	| propertyName oldNames newNames |
	propertyName := 'classinstvars'.
	oldNames := self propertyAt: propertyName.
	newNames := handle class instVarNames collect: [:each | each asString].
	oldNames = newNames ifFalse: [self propertyAt: propertyName put: newNames]
]

{ #category : 'private-updating' }
CypGsLoadedClass >> updateClassNameFromClass [

	| oldName newName |
	oldName := name.
	newName := handle name asString.
	oldName = newName
		ifFalse: 
			[self name: newName.
			self propertyAt: 'name' put: name]
]

{ #category : 'private-updating' }
CypGsLoadedClass >> updateClassTypeFromClass [
	"For GemStone, the valid values are:
	bytes
	indexable
	immediate
	nsc
	No property is recorded for non-indexable pointer objects."

	| propertyName oldValue newValue changed |
	propertyName := '_gs_type'.
	oldValue := self propertyAt: propertyName.
	newValue := handle isBytesOrSpecial
				ifTrue: [handle isBytes ifTrue: ['bytes'] ifFalse: ['immediate']]
				ifFalse: 
					[handle isNsc
						ifTrue: ['nsc']
						ifFalse: 
							[handle isVariable
								ifTrue: ['indexable']
								ifFalse: [self absentToken	"non-indexable pointer object"]]].

	"Since the absent token is not equal to any other valid value, we can skip the identity check in this case."
	oldValue = newValue
		ifFalse: 
			[newValue == self absentToken
				ifTrue: [self removeProperty: propertyName]
				ifFalse: [self propertyAt: propertyName put: newValue]]
]

{ #category : 'private-updating' }
CypGsLoadedClass >> updateClassVariableNamesFromClass [
	"Order of variable names does not matter to Cypress (at least not for GemStone) so we keep the names sorted."

	| propertyName oldNames newNames |
	propertyName := 'classvars'.
	oldNames := self propertyAt: propertyName.
	newNames := (handle classVarNames collect: [:each | each asString])
				asSortedCollection.
	oldNames = newNames ifFalse: [self propertyAt: propertyName put: newNames]
]

{ #category : 'private-updating' }
CypGsLoadedClass >> updateCommentFromClass [

	| propertyName oldValue newValue absentToken |
	propertyName := 'comment'.
	absentToken := self absentToken.
	oldValue := self propertyAt: propertyName.
	newValue := (handle _extraDictAt: #comment) ifNil: [absentToken].
	(newValue ~~ absentToken and: [newValue isEmpty])
		ifTrue: [newValue := absentToken].

	"Absent token must be compared by identity, because a comment of '<absent>' would be equal to no comment."
	(oldValue == absentToken | (newValue == absentToken)
		ifTrue: [oldValue == newValue]
		ifFalse: [oldValue = newValue])
			ifFalse: 
				[newValue == absentToken
					ifTrue: [self removeProperty: propertyName]
					ifFalse: [self propertyAt: propertyName put: newValue]]
]

{ #category : 'private-updating' }
CypGsLoadedClass >> updateForCategoryChange [
	"Part of updating from image categories.
	If my category property no longer matches the package I'm in, move me to that package."

	| oldPackageName newPackageName |
	oldPackageName := loadedPackage name.
	newPackageName := self propertyAt: 'category'.
	oldPackageName = newPackageName
		ifFalse: 
			[| newPackage |
			loadedPackage removeLoadedClass: self.
			"Could need to create the new package."
			newPackage := Cypress existingOrNewLoadedPackageNamed: newPackageName.
			newPackage addLoadedClass: self]
]

{ #category : 'updating' }
CypGsLoadedClass >> updateFromImageCategory [
	"My information about a class may be out of date. Update my information about the class.
	Things that might cause me to need updating:
	* The class may have been deleted or renamed.
	* The class may have been replaced by a new version.
 	* The class may have changed its category, which will move it to a different package.
	* The class comment may have changed."

	self handleClassDeletionOrNewVersion
		ifTrue: 
			[self
				updatePropertiesFromClass;
				updateForCategoryChange]
		ifFalse: 
			["Class has been deleted and the deletion handled"
			]
]

{ #category : 'private-updating' }
CypGsLoadedClass >> updateInstvarNamesFromClass [
	"Order of instvars does not matter to Cypress (at least not in GemStone) so we keep the instvar names sorted."

	| propertyName oldNames newNames |
	propertyName := 'instvars'.
	oldNames := self propertyAt: propertyName.
	newNames := handle instVarNames collect: [:each | each asString].
	oldNames = newNames ifFalse: [self propertyAt: propertyName put: newNames]
]

{ #category : 'private-updating' }
CypGsLoadedClass >> updateOptionsFromClass [
	"Options are GemStone-specific. If the class has no options, the property is absent.
	Otherwise, the property is a sorted array of strings. The valid values are:

	dbTransient
	disallowGciStore
	instancesInvariant
	instancesNonPersistent
	modifiable
	subclassesDisallowed
	traverseByCallback

	Not all combinations are legal, but we don't have to worry about that here, since we're getting the values
	from the image, and the image is trusted to only have valid values."

	| propertyName oldValue newValue changed |
	propertyName := '_gs_options'.
	oldValue := self propertyAt: propertyName.
	newValue := (handle _optionsArray collect: [:option | option asString])
				asSortedCollection.
	newValue isEmpty ifTrue: [newValue := self absentToken].

	"Since the absent token is not equal to any other valid value, we can skip the identity check in this case."
	oldValue = newValue
		ifFalse: 
			[newValue == self absentToken
				ifTrue: [self removeProperty: propertyName]
				ifFalse: [self propertyAt: propertyName put: newValue]]
]

{ #category : 'private-updating' }
CypGsLoadedClass >> updatePoolDictionaryNamesFromClass [
	"Order of pool dictionaries *does* matter in GemStone, so we keep the names in the order given in the image."

	| propertyName oldNames newNames |
	propertyName := 'pools'.
	oldNames := self propertyAt: propertyName.
	newNames := (handle sharedPools collect: [:each | each name asString])
				asSortedCollection.
	oldNames = newNames ifFalse: [self propertyAt: propertyName put: newNames]
]

{ #category : 'private-updating' }
CypGsLoadedClass >> updatePropertiesFromClass [
	"Update my properties from the class. Leave unknown properties alone, they may apply to another platform."

	self
		updateSymbolDictionaryFromClass;
		updateClassNameFromClass;
		updateSuperclassNameFromClass;
		updateInstvarNamesFromClass;
		updateClassInstvarNamesFromClass;
		updateClassVariableNamesFromClass;
		updatePoolDictionaryNamesFromClass;
		updateClassTypeFromClass;
		updateOptionsFromClass;
		updateCommentFromClass;
		updateCategoryFromClass
]

{ #category : 'private-updating' }
CypGsLoadedClass >> updateSuperclassNameFromClass [

	| oldName newName |
	oldName := self propertyAt: 'super'.
	newName := handle superclass ifNotNil: [:super | super name asString].
	oldName = newName ifFalse: [self propertyAt: 'super' put: newName]
]

{ #category : 'private-updating' }
CypGsLoadedClass >> updateSymbolDictionaryFromClass [

	| dicts newName oldName |
	dicts := self class symbolDictsForClass: handle.
	dicts size = 0
		ifTrue: [self error: 'Class not found by name in symbol list.'].
	dicts size > 1
		ifTrue: 
			[CypNotification
				signal: 'Class resolved in more than one SymbolDictionary. Using first found.'].
	newName := dicts first name.
	oldName := self propertyAt: 'SymbolDictionary'.
	oldName = newName ifFalse: [self symbolDictionaryName: newName]
]
