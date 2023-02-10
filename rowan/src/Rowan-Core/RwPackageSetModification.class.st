"
A modification showing the difference between two PackageSets. The elements are packages. Key is package name.
movedClasses are classes that have moved from one package to another within the package set.
movedMethods are methods that have moved from a class definition to an extension of that class, or vice versa, or between two extensions of the same class.
"
Class {
	#name : 'RwPackageSetModification',
	#superclass : 'RwEntitySetModification',
	#category : 'Rowan-Core'
}

{ #category : 'visiting' }
RwPackageSetModification >> acceptVisitor: aVisitor [

	^aVisitor visitPackageSetModification: self
]

{ #category : 'private - moves' }
RwPackageSetModification >> addMethodsAddedByClassesOrExtensionsModification: classesOrExtensionsModification inPackage: package toDictionary: dictionary [

	classesOrExtensionsModification elementsModified do: 
			[:classModification |
			| classMethodsModification instanceMethodsModification |
			classMethodsModification := classModification classMethodsModification.
			instanceMethodsModification := classModification
						instanceMethodsModification.
			classMethodsModification elementsModified do: 
					[:methodModification |
					methodModification before
						ifNil: 
							[| newMethod |
							newMethod := methodModification after.
							dictionary at: classModification key -> true -> newMethod key
								put: (RwMethodAdditionOrRemoval
										packageDefinition: package
										classDefinitionOrExtension: classModification after
										methodKey: newMethod key
										isMeta: true
										methodsModification: classMethodsModification)]].
			instanceMethodsModification elementsModified do: 
					[:methodModification |
					methodModification before
						ifNil: 
							[| newMethod |
							newMethod := methodModification after.
							dictionary at: classModification key -> false -> newMethod key
								put: (RwMethodAdditionOrRemoval
										packageDefinition: package
										classDefinitionOrExtension: classModification after
										methodKey: newMethod key
										isMeta: false
										methodsModification: instanceMethodsModification)]]]
]

{ #category : 'private - moves' }
RwPackageSetModification >> addMethodsAddedByPackageModification: packageModification toDictionary: dictionary [

	self
		addMethodsAddedByClassesOrExtensionsModification: packageModification
				classesModification
		inPackage: packageModification after
		toDictionary: dictionary.
	self
		addMethodsAddedByClassesOrExtensionsModification: packageModification
				classExtensionsModification
		inPackage: packageModification after
		toDictionary: dictionary
]

{ #category : 'private - moves' }
RwPackageSetModification >> addMethodsRemovedByClassesOrExtensionsModification: classesOrExtensionsModification inPackage: package toDictionary: dictionary [

	classesOrExtensionsModification elementsModified do: 
			[:classModification |
			| classMethodsModification instanceMethodsModification |
			classMethodsModification := classModification classMethodsModification.
			instanceMethodsModification := classModification
						instanceMethodsModification.
			classMethodsModification elementsModified do: 
					[:methodModification |
					methodModification after
						ifNil: 
							[| oldMethod |
							oldMethod := methodModification before.
 							dictionary at: classModification key -> true -> oldMethod key
								put: (RwMethodAdditionOrRemoval
										packageDefinition: package
										classDefinitionOrExtension: classModification before
										methodKey: oldMethod key
										isMeta: true
										methodsModification: classMethodsModification)]].
			instanceMethodsModification elementsModified do: 
					[:methodModification |
					methodModification after
						ifNil: 
							[| oldMethod |
							oldMethod := methodModification before.
						dictionary at: classModification key -> false -> oldMethod key
								put: (RwMethodAdditionOrRemoval
										packageDefinition: package
										classDefinitionOrExtension: classModification after
										methodKey: oldMethod key
										isMeta: false
										methodsModification: instanceMethodsModification)]]]
]

{ #category : 'private - moves' }
RwPackageSetModification >> addMethodsRemovedByPackageModification: packageModification toDictionary: dictionary [

	self
		addMethodsRemovedByClassesOrExtensionsModification: packageModification
				classesModification
		inPackage: packageModification before
		toDictionary: dictionary.
	self
		addMethodsRemovedByClassesOrExtensionsModification: packageModification
				classExtensionsModification
		inPackage: packageModification before
		toDictionary: dictionary
]

{ #category : 'enumerating' }
RwPackageSetModification >> classesModificationAndPackageModificationAndProjectModificationDo: aBlock [

	"enumerate the classesModification in the packagesModification in the projectModification in the receiver.
		The classesModification, packageModification are arguments to the block. "

	elementsModified
		do: [ :packageModification | aBlock cull: packageModification classesModification cull: packageModification  ]

]

{ #category : 'enumerating' }
RwPackageSetModification >> classesModificationAndPackageModificationDo: aBlock [

	"enumerate the classesModification in the packagesModification in the receiver.
		The classesModification and ppackageModification are arguments to the block. "

	elementsModified
		do: [ :packageModification | aBlock cull: packageModification classesModification cull: packageModification  ]

]

{ #category : 'private - moves' }
RwPackageSetModification >> findAddedClasses [

	| addedClasses |
	addedClasses := Dictionary new.
	elementsModified do: 
			[:packageModification |
			| classesModification |
			classesModification := packageModification classesModification.
			classesModification elementsModified do: 
					[:classModification |
					classModification before isEmpty
						ifTrue: 
							[| newClass |
							newClass := classModification after.
							addedClasses at: newClass key
								put: (RwClassAdditionOrRemoval
										packageDefinition: packageModification before
										classKey: newClass key
										classesModification: classesModification)]]].
	^addedClasses
]

{ #category : 'private - moves' }
RwPackageSetModification >> findAddedMethods [
	"Answer a dictionary of added methods."

	| addedMethods |
	addedMethods := Dictionary new.
	elementsModified do: 
			[:packageModification |
			self addMethodsAddedByPackageModification: packageModification
				toDictionary: addedMethods].
	^addedMethods
]

{ #category : 'private - moves' }
RwPackageSetModification >> findRemovedClasses [

	| removedClasses |
	removedClasses := Dictionary new.
	elementsModified do: 
			[:packageModification |
			| classesModification |
			classesModification := packageModification classesModification.
			classesModification elementsModified do: 
					[:classModification |
					classModification after isEmpty
						ifTrue: 
							[| oldClass |
							oldClass := classModification before.
							removedClasses at: oldClass key
								put: (RwClassAdditionOrRemoval
										packageDefinition: packageModification after
										classKey: oldClass key
										classesModification: classesModification)]]].
	^removedClasses
]

{ #category : 'private - moves' }
RwPackageSetModification >> findRemovedMethods [
	"Answer a dictionary of removed methods."

	| removedMethods |
	removedMethods := Dictionary new.
	elementsModified do: 
			[:packageModification |
			self addMethodsRemovedByPackageModification: packageModification
				toDictionary: removedMethods].
	^removedMethods
]

{ #category : 'initialization' }
RwPackageSetModification >> initialize [

	super initialize.
	movedClasses := Set new.
	movedMethods := Set new
]

{ #category : 'private - moves' }
RwPackageSetModification >> updateForClassMoveFrom: removal to: addition [
	"Transform the given removal and addition to a move."

	| oldDefinition newDefinition classModification |
	oldDefinition := (removal classesModification
				modificationOf: removal classKey) before.
	newDefinition := (addition classesModification
				modificationOf: addition classKey) after.

	"Delete the removal and the addition."
	removal classesModification removeModificationOf: removal classKey.
	addition classesModification removeModificationOf: addition classKey.

	"Record the move."
	movedClasses add: (RwClassMove
				classBefore: oldDefinition
				classAfter: newDefinition
				packageBefore: removal packageDefinition
				packageAfter: addition packageDefinition).

	"Does the class have other modifications that need to be recorded?"
	classModification := newDefinition compareAgainstBase: oldDefinition.
	classModification isEmpty
		ifFalse: 
			[addition classesModification addElementModification: classModification]
]

{ #category : 'moves' }
RwPackageSetModification >> updateForClassMoves [

	| classAdditions classRemovals |
	classAdditions := self findAddedClasses.
	classRemovals := self findRemovedClasses.

	"Any keys that match between added and removed should be considered a move."
	classAdditions keysAndValuesDo: 
			[:key :addition |
			| removal |
			removal := classRemovals at: key ifAbsent: [nil].
			removal ifNotNil: [self updateForClassMoveFrom: removal to: addition]]
]

{ #category : 'private - moves' }
RwPackageSetModification >> updateForMethodMoveFrom: removal to: addition [
	"Transform the given removal and addition to a move."

	| oldDefinition newDefinition methodModification |
	oldDefinition := (removal methodsModification
				modificationOf: removal methodKey) before.
	newDefinition := (addition methodsModification
				modificationOf: addition methodKey) after.

	"Delete the removal and the addition."
	removal methodsModification removeModificationOf: removal methodKey.
	addition methodsModification removeModificationOf: addition methodKey.

	"Record the move."
	movedMethods add: (RwMethodMove
				methodBefore: oldDefinition
				methodAfter: newDefinition
				classOrExtensionBefore: removal classDefinitionOrExtension
				classOrExtensionAfter: addition classDefinitionOrExtension
				packageBefore: removal packageDefinition
				packageAfter: addition packageDefinition
				isMeta: addition isMeta).

	"Does the method have other modifications that need to be recorded?"
	methodModification := newDefinition compareAgainstBase: oldDefinition.
	methodModification isEmpty
		ifFalse: 
			[addition methodsModification addElementModification: methodModification]
]

{ #category : 'moves' }
RwPackageSetModification >> updateForMethodMoves [
	"Methods that have been moved between packages will initially show up as a remove and an add rather than a move.
	Find moved methods and correct the structure."

	| methodAdditions methodRemovals |
	methodAdditions := self findAddedMethods.
	methodRemovals := self findRemovedMethods.	"Any keys that match between added and removed should be considered a move."
	methodAdditions
		keysAndValuesDo: [ :key :addition | 
			| removal |
			removal := methodRemovals at: key ifAbsent: [ nil ].
			removal ifNotNil: [ self updateForMethodMoveFrom: removal to: addition ] ]
]
