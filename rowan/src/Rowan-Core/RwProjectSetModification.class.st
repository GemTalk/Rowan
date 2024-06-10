"
A modification showing the difference between two PackageSets. The elements are packages. Key is package name.
movedClasses are classes that have moved from one package to another within the package set.
movedMethods are methods that have moved from a class definition to an extension of that class, or vice versa, or between two extensions of the same class.
"
Class {
	#name : 'RwProjectSetModification',
	#superclass : 'RwEntitySetModification',
	#category : 'Rowan-Core'
}

{ #category : 'visiting' }
RwProjectSetModification >> acceptVisitor: aVisitor [

	^ aVisitor visitProjecteSetModification: self
]

{ #category : 'private - moves' }
RwProjectSetModification >> addMethodsAddedByClassesOrExtensionsModification: classesOrExtensionsModification inPackage: package inProject: project toDictionary: dictionary [

	classesOrExtensionsModification elementsModified do: 
			[:classModification |
			| classMethodsModification instanceMethodsModification |
			classMethodsModification := classModification classMethodsModification.
			instanceMethodsModification := classModification
						instanceMethodsModification.
			classMethodsModification elementsModified do: 
					[:methodModification |
					methodModification before key
						ifNil: 
							[| newMethod |
							newMethod := methodModification after.
							dictionary at: classModification key -> true -> newMethod key
								put: (RwMethodAdditionOrRemoval
										projectDefinition: project
										packageDefinition: package
										classDefinitionOrExtension: classModification after
										methodKey: newMethod key
										isMeta: true
										methodsModification: classMethodsModification)]].
			instanceMethodsModification elementsModified do: 
					[:methodModification |
					methodModification before key
						ifNil: 
							[| newMethod |
							newMethod := methodModification after.
							dictionary at: classModification key -> false -> newMethod key
								put: (RwMethodAdditionOrRemoval
										projectDefinition: project
										packageDefinition: package
										classDefinitionOrExtension: classModification after
										methodKey: newMethod key
										isMeta: false
										methodsModification: instanceMethodsModification)]]]

]

{ #category : 'private - moves' }
RwProjectSetModification >> addMethodsAddedByPackageModification: packageModification inProject: projectModification toDictionary: dictionary [

	packageModification after key notNil
	ifTrue: [ 
		"ignore additions for packages that have been removed"
		self
			addMethodsAddedByClassesOrExtensionsModification: packageModification
					classesModification
			inPackage: packageModification after
			inProject: projectModification after
			toDictionary: dictionary.
		self
			addMethodsAddedByClassesOrExtensionsModification: packageModification
					classExtensionsModification
			inPackage: packageModification after
			inProject: projectModification after
			toDictionary: dictionary]

]

{ #category : 'private - moves' }
RwProjectSetModification >> addMethodsRemovedByClassesOrExtensionsModification: classesOrExtensionsModification inPackage: package inProject: project toDictionary: dictionary [

	classesOrExtensionsModification elementsModified do: 
			[:classModification |
			| classMethodsModification instanceMethodsModification |
			classMethodsModification := classModification classMethodsModification.
			instanceMethodsModification := classModification
						instanceMethodsModification.
			classMethodsModification elementsModified do: 
					[:methodModification |
					methodModification after key
						ifNil: 
							[| oldMethod |
							oldMethod := methodModification before.
							dictionary at: classModification key -> true -> oldMethod key
								put: (RwMethodAdditionOrRemoval
										projectDefinition: project
										packageDefinition: package
										classDefinitionOrExtension: classModification before
										methodKey: oldMethod key
										isMeta: true
										methodsModification: classMethodsModification)]].
			instanceMethodsModification elementsModified do: 
					[:methodModification |
					methodModification after key
						ifNil: 
							[| oldMethod |
							oldMethod := methodModification before.
							dictionary at: classModification key -> false -> oldMethod key
								put: (RwMethodAdditionOrRemoval
										projectDefinition: project
										packageDefinition: package
										classDefinitionOrExtension: classModification after
										methodKey: oldMethod key
										isMeta: false
										methodsModification: instanceMethodsModification)]]]

]

{ #category : 'private - moves' }
RwProjectSetModification >> addMethodsRemovedByPackageModification: packageModification inProject: projectModification toDictionary: dictionary [

	packageModification before key notNil
	ifTrue: [ 
		"ignore removals for packages that have been added"
		self
			addMethodsRemovedByClassesOrExtensionsModification: packageModification
					classesModification
			inPackage: packageModification before
			inProject: projectModification before
			toDictionary: dictionary.
		self
			addMethodsRemovedByClassesOrExtensionsModification: packageModification
					classExtensionsModification
			inPackage: packageModification before
			inProject: projectModification before
			toDictionary: dictionary ]

]

{ #category : 'private - moves' }
RwProjectSetModification >> addTraitMethodsAddedByPackageModification: packageModification inProject: projectModification toDictionary: dictionary [

	packageModification after key notNil
	ifTrue: [ 
	packageModification traitsModification elementsModified do: 
			[:traitModification |
			| classMethodsModification instanceMethodsModification |
			classMethodsModification := traitModification classMethodsModification.
			instanceMethodsModification := traitModification
						instanceMethodsModification.
			classMethodsModification elementsModified do: 
					[:methodModification |
					methodModification before key
						ifNil: 
							[| newMethod |
							newMethod := methodModification after.
							dictionary at: traitModification key -> true -> newMethod key
								put: (RwTraitMethodAdditionOrRemoval
										projectDefinition: projectModification after
										packageDefinition: packageModification after
										traitDefinition: traitModification after
										methodKey: newMethod key
										isMeta: true
										methodsModification: classMethodsModification)]].
			instanceMethodsModification elementsModified do: 
					[:methodModification |
					methodModification before key
						ifNil: 
							[| newMethod |
							newMethod := methodModification after.
							dictionary at: traitModification key -> false -> newMethod key
								put: (RwTraitMethodAdditionOrRemoval
										projectDefinition: projectModification after
										packageDefinition: packageModification after
										traitDefinition: traitModification after
										methodKey: newMethod key
										isMeta: false
										methodsModification: instanceMethodsModification)]]]
]
]

{ #category : 'private - moves' }
RwProjectSetModification >> addTraitMethodsRemovedByPackageModification: packageModification inProject: projectModification toDictionary: dictionary [

	packageModification before key notNil
	ifTrue: [ 
		"ignore removals for packages that have been added"
		self
			addTraitMethodsRemovedByTraitsModification: packageModification
					traitsModification
			inPackage: packageModification before
			inProject: projectModification before
			toDictionary: dictionary ]
]

{ #category : 'private - moves' }
RwProjectSetModification >> addTraitMethodsRemovedByTraitsModification: traitsModification inPackage: package inProject: project toDictionary: dictionary [

	traitsModification elementsModified do: 
			[:traitModification |
			| classMethodsModification instanceMethodsModification |
			classMethodsModification := traitModification classMethodsModification.
			instanceMethodsModification := traitModification
						instanceMethodsModification.
			classMethodsModification elementsModified do: 
					[:methodModification |
					methodModification after key
						ifNil: 
							[| oldMethod |
							oldMethod := methodModification before.
							dictionary at: traitModification key -> true -> oldMethod key
								put: (RwTraitMethodAdditionOrRemoval
										projectDefinition: project
										packageDefinition: package
										traitDefinition: traitModification before
										methodKey: oldMethod key
										isMeta: true
										methodsModification: classMethodsModification)]].
			instanceMethodsModification elementsModified do: 
					[:methodModification |
					methodModification after key
						ifNil: 
							[| oldMethod |
							oldMethod := methodModification before.
							dictionary at: traitModification key -> false -> oldMethod key
								put: (RwTraitMethodAdditionOrRemoval
										projectDefinition: project
										packageDefinition: package
										traitDefinition: traitModification after
										methodKey: oldMethod key
										isMeta: false
										methodsModification: instanceMethodsModification)]]]
]

{ #category : 'enumerating' }
RwProjectSetModification >> classesModificationAndPackageModificationAndProjectModificationDo: aBlock [

	"enumerate the classesModification in the packagesModification in the projectModification in the receiver.
		The classesModification, packageModification, and projectModification are arguments to the block. "

	elementsModified
		do: [ :projectModification | 
			projectModification packagesModification elementsModified
				do: [ :packageModification | aBlock cull: packageModification classesModification cull: packageModification cull: projectModification ] ]

]

{ #category : 'private - moves' }
RwProjectSetModification >> findAddedClasses [

	| addedClasses |
	addedClasses := Dictionary new.
	elementsModified do: 
			[:projectModification |
			| packagesModification |
			packagesModification := projectModification packagesModification.
			packagesModification elementsModified do: 
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
											projectDefinition: projectModification after
											packageDefinition: packageModification after
											classKey: newClass key
											classesModification: classesModification)]]]].
	^addedClasses

]

{ #category : 'private - moves' }
RwProjectSetModification >> findAddedMethods [
	"Answer a dictionary of added methods."

	| addedMethods |
	addedMethods := Dictionary new.
	elementsModified do: 
			[:projectModification |
			| packagesModification |
			packagesModification := projectModification packagesModification.
			packagesModification elementsModified do: 
				[:packageModification |
				self addMethodsAddedByPackageModification: packageModification
					inProject: projectModification
					toDictionary: addedMethods] ].
	^addedMethods

]

{ #category : 'private - moves' }
RwProjectSetModification >> findAddedPackages [
	| addedPackages |
	addedPackages := Dictionary new.
	elementsModified
		do: [ :projectModification | 
			| packagesModification |
			packagesModification := projectModification packagesModification.
			packagesModification elementsModified
				do: [ :packageModification | 
					packageModification before isEmpty
						ifTrue: [ 
							| newPackage |
							newPackage := packageModification after.
							addedPackages
								at: newPackage key
								put:
									(RwPackageAdditionOrRemoval
										projectDefinition: projectModification after
										packageDefinition: packageModification after
										packageKey: newPackage key
										packagesModification: packagesModification) ] ] ].
	^ addedPackages
]

{ #category : 'private - moves' }
RwProjectSetModification >> findAddedTraitMethods [
	"Answer a dictionary of added methods."

	| addedTraitMethods |
	addedTraitMethods := Dictionary new.
	elementsModified
		do: [ :projectModification | 
			| packagesModification |
			packagesModification := projectModification packagesModification.
			packagesModification elementsModified
				do: [ :packageModification | 
					self
						addTraitMethodsAddedByPackageModification: packageModification
						inProject: projectModification
						toDictionary: addedTraitMethods ] ].
	^ addedTraitMethods
]

{ #category : 'private - moves' }
RwProjectSetModification >> findAddedTraits [

	| addedTraits |
	addedTraits := Dictionary new.
	elementsModified do: 
			[:projectModification |
			| packagesModification |
			packagesModification := projectModification packagesModification.
			packagesModification elementsModified do: 
				[:packageModification |
				| traitsModification |
				traitsModification := packageModification traitsModification.
				traitsModification elementsModified do: 
						[:traitModification |
						traitModification before isEmpty
							ifTrue: 
								[| newTrait |
								newTrait := traitModification after.
								addedTraits at: newTrait key
									put: (RwTraitAdditionOrRemoval
											projectDefinition: projectModification after
											packageDefinition: packageModification after
											traitKey: newTrait key
											traitsModification: traitsModification)]]]].
	^addedTraits
]

{ #category : 'private - moves' }
RwProjectSetModification >> findRemovedClasses [

	| removedClasses |
	removedClasses := Dictionary new.
	elementsModified do: 
			[:projectModification |
			| packagesModification |
			packagesModification := projectModification packagesModification.
			packagesModification elementsModified do: 
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
											projectDefinition: projectModification before
											packageDefinition: packageModification before
											classKey: oldClass key
											classesModification: classesModification)]]]].
	^removedClasses

]

{ #category : 'private - moves' }
RwProjectSetModification >> findRemovedMethods [
	"Answer a dictionary of removed methods."

	| removedMethods |
	removedMethods := Dictionary new.
	elementsModified do: 
			[:projectModification |
			| packagesModification |
			packagesModification := projectModification packagesModification.
			packagesModification elementsModified do: 
				[:packageModification |
				self addMethodsRemovedByPackageModification: packageModification
					inProject: projectModification
					toDictionary: removedMethods]].
	^removedMethods

]

{ #category : 'private - moves' }
RwProjectSetModification >> findRemovedPackages [
	| removedPackages |
	removedPackages := Dictionary new.
	elementsModified
		do: [ :projectModification | 
			| packagesModification |
			packagesModification := projectModification packagesModification.
			packagesModification elementsModified
				do: [ :packageModification | 
					packageModification after isEmpty
						ifTrue: [ 
							| oldPackage |
							oldPackage := packageModification before.
							removedPackages
								at: oldPackage key
								put:
									(RwPackageAdditionOrRemoval
										projectDefinition: projectModification before
										packageDefinition: packageModification before
										packageKey: oldPackage key
										packagesModification: packagesModification) ] ] ].
	^ removedPackages
]

{ #category : 'private - moves' }
RwProjectSetModification >> findRemovedTraitMethods [
	"Answer a dictionary of removed methods."

	| removedMethods |
	removedMethods := Dictionary new.
	elementsModified do: 
			[:projectModification |
			| packagesModification |
			packagesModification := projectModification packagesModification.
			packagesModification elementsModified do: 
				[:packageModification |
				self addTraitMethodsRemovedByPackageModification: packageModification
					inProject: projectModification
					toDictionary: removedMethods]].
	^removedMethods

]

{ #category : 'private - moves' }
RwProjectSetModification >> findRemovedTraits [

	| removedTraits |
	removedTraits := Dictionary new.
	elementsModified do: 
			[:projectModification |
			| packagesModification |
			packagesModification := projectModification packagesModification.
			packagesModification elementsModified do: 
				[:packageModification |
				| traitsModification |
				traitsModification := packageModification traitsModification.
				traitsModification elementsModified do: 
						[:traitModification |
						traitModification after isEmpty
							ifTrue: 
								[| oldTrait |
								oldTrait := traitModification before.
								removedTraits at: oldTrait key
									put: (RwTraitAdditionOrRemoval
											projectDefinition: projectModification before
											packageDefinition: packageModification before
											traitKey: oldTrait key
											traitsModification: traitsModification)]]]].
	^removedTraits
]

{ #category : 'initialization' }
RwProjectSetModification >> initialize [
	super initialize.
	movedPackages := Set new.
	movedClasses := Set new.
	movedMethods := Set new.
	movedTraits := Set new.
	movedTraitMethods := Set new
]

{ #category : 'testing' }
RwProjectSetModification >> isEmpty [
	^ movedMethods isEmpty
		and: [ 
			movedClasses isEmpty
				and: [ movedTraitMethods isEmpty and: [ movedTraits isEmpty and: [ super isEmpty ] ] ] ]
]

{ #category : 'private - moves' }
RwProjectSetModification >> updateForClassMoveFrom: removal to: addition [
	"Transform the given removal and addition to a move."

	| oldDefinition newDefinition classModification |
	removal
		ifNil: [ 
			"not a move"
			^ self ].
	addition
		ifNil: [ 
			"not a move"
			^ self ].
	oldDefinition := (removal classesModification modificationOf: removal classKey)
		before.
	newDefinition := (addition classesModification
		modificationOf: addition classKey) after.	"Delete the removal and the addition."
	removal classesModification removeModificationOf: removal classKey.
	addition classesModification removeModificationOf: addition classKey.	"Record the move."
	movedClasses
		add:
			(RwClassMove
				classBefore: oldDefinition
				classAfter: newDefinition
				packageBefore: removal packageDefinition
				packageAfter: addition packageDefinition
				projectBefore: removal projectDefinition
				projectAfter: addition projectDefinition).	"Does the class have other modifications that need to be recorded?"
	classModification := newDefinition compareAgainstBase: oldDefinition.
	classModification isEmpty
		ifFalse: [ addition classesModification addElementModification: classModification ]
]

{ #category : 'moves' }
RwProjectSetModification >> updateForClassMoves [

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
RwProjectSetModification >> updateForMethodMoveFrom: removal to: addition isMeta: isMeta [
	"Transform the given removal and addition to a move."

	| oldDefinition newDefinition methodModification |
	removal
		ifNil: [ 
			"not a move"
			^ self ].
	addition
		ifNil: [ 
			"not a move"
			^ self ].
	oldDefinition := (removal methodsModification modificationOf: removal methodKey)
		before.
	newDefinition := (addition methodsModification
		modificationOf: addition methodKey) after.
	"Delete the removal and the addition."
	removal methodsModification removeModificationOf: removal methodKey.
	addition methodsModification removeModificationOf: addition methodKey.
	"Record the move."
	movedMethods
		add:
			(RwMethodMove
				methodBefore: oldDefinition
				methodAfter: newDefinition
				classOrExtensionBefore: removal classDefinitionOrExtension
				classOrExtensionAfter: addition classDefinitionOrExtension
				packageBefore: removal packageDefinition
				packageAfter: addition packageDefinition
				projectBefore: removal projectDefinition
				projectAfter: addition projectDefinition
				isMeta: addition isMeta).
	"Does the method have other modifications that need to be recorded?"
	methodModification := newDefinition compareAgainstBase: oldDefinition.
	methodModification
		isMeta: isMeta;
		classDefinition: addition classDefinitionOrExtension.
	methodModification isEmpty
		ifFalse: [ addition methodsModification addElementModification: methodModification ]
]

{ #category : 'moves' }
RwProjectSetModification >> updateForMethodMoves [
	"Methods that have been moved between packages will initially show up as a remove and an add rather than a move.
	Find moved methods and correct the structure."

	| methodAdditions methodRemovals |
	methodAdditions := self findAddedMethods.
	methodRemovals := self findRemovedMethods.
	"Any keys that match between added and removed should be considered a move."
	methodAdditions keysAndValuesDo: 
			[:key :addition |
			| removal |
			removal := methodRemovals at: key ifAbsent: [nil].
			removal ifNotNil: [ self updateForMethodMoveFrom: removal to: addition isMeta: key key value]]
]

{ #category : 'private - moves' }
RwProjectSetModification >> updateForPackageMoveFrom: removal to: addition [
	"Transform the given removal and addition to a move."

	| oldDefinition newDefinition packageModification |
	removal
		ifNil: [ 
			"not a move"
			^ self ].
	addition
		ifNil: [ 
			"not a move"
			^ self ].
	oldDefinition := (removal packagesModification
		modificationOf: removal packageKey) before.
	newDefinition := (addition packagesModification
		modificationOf: addition packageKey) after.
	removal packagesModification removeModificationOf: removal packageKey.	"Delete the removal."
	addition packagesModification removeModificationOf: addition packageKey.	"Delete theaddition."

	movedPackages
		add:
			(RwPackageMove
				packageBefore: oldDefinition
				packageAfter: newDefinition
				projectBefore: removal projectDefinition
				projectAfter: addition projectDefinition).	"Record the move."
	packageModification := newDefinition compareAgainstBase: oldDefinition.
	packageModification isEmpty
		ifFalse: [ 
			"Does the package have other modifications that need to be recorded?"
			addition packagesModification addElementModification: packageModification ]
]

{ #category : 'moves' }
RwProjectSetModification >> updateForPackageMoves [

	| packageAdditions packageRemovals |
	packageAdditions := self findAddedPackages.
	packageRemovals := self findRemovedPackages.

	"Any keys that match between added and removed should be considered a move."
	packageAdditions keysAndValuesDo: 
			[:key :addition |
			| removal |
			removal := packageRemovals at: key ifAbsent: [nil].
			removal ifNotNil: [self updateForPackageMoveFrom: removal to: addition]]
]

{ #category : 'private - moves' }
RwProjectSetModification >> updateForTraitMethodMoveFrom: removal to: addition isMeta: isMeta [
	"Transform the given removal and addition to a move."

	| oldDefinition newDefinition methodModification |
	removal
		ifNil: [ 
			"not a move"
			^ self ].
	addition
		ifNil: [ 
			"not a move"
			^ self ].
	oldDefinition := (removal methodsModification modificationOf: removal methodKey)
		before.
	newDefinition := (addition methodsModification
		modificationOf: addition methodKey) after.	"Delete the removal and the addition."
	removal methodsModification removeModificationOf: removal methodKey.
	addition methodsModification removeModificationOf: addition methodKey.	"Record the move."
	movedMethods
		add:
			(RwTraitMethodMove
				methodBefore: oldDefinition
				methodAfter: newDefinition
				traitBefore: removal traitDefinition
				traitAfter: addition traitDefinition
				packageBefore: removal packageDefinition
				packageAfter: addition packageDefinition
				projectBefore: removal projectDefinition
				projectAfter: addition projectDefinition
				isMeta: addition isMeta).	"Does the method have other modifications that need to be recorded?"
	methodModification := newDefinition compareAgainstBase: oldDefinition.
	methodModification
		isMeta: isMeta;
		traitDefinition: addition traitDefinition.
	methodModification isEmpty
		ifFalse: [ addition methodsModification addElementModification: methodModification ]
]

{ #category : 'moves' }
RwProjectSetModification >> updateForTraitMethodMoves [
	"Trait methods that have been moved between packages will initially show up as a remove and an add rather than a move.
	Find moved trait methods and correct the structure."

	| methodAdditions methodRemovals |
	methodAdditions := self findAddedTraitMethods.
	methodRemovals := self findRemovedTraitMethods.	"Any keys that match between added and removed should be considered a move."
	methodAdditions
		keysAndValuesDo: [ :key :addition | 
			| removal |
			removal := methodRemovals at: key ifAbsent: [ nil ].
			removal
				ifNotNil: [ self updateForTraitMethodMoveFrom: removal to: addition isMeta: key key value ] ]
]

{ #category : 'private - moves' }
RwProjectSetModification >> updateForTraitMoveFrom: removal to: addition [
	"Transform the given removal and addition to a move."

	| oldDefinition newDefinition traitModification |
	removal
		ifNil: [ 
			"not a move"
			^ self ].
	addition
		ifNil: [ 
			"not a move"
			^ self ].
	oldDefinition := (removal traitsModification modificationOf: removal traitKey)
		before.
	newDefinition := (addition traitsModification modificationOf: addition traitKey)
		after.	"Delete the removal and the addition."
	removal traitsModification removeModificationOf: removal traitKey.
	addition traitsModification removeModificationOf: addition traitKey.	"Record the move."
	movedTraits
		add:
			(RwTraitMove
				traitBefore: oldDefinition
				traitAfter: newDefinition
				packageBefore: removal packageDefinition
				packageAfter: addition packageDefinition
				projectBefore: removal projectDefinition
				projectAfter: addition projectDefinition).	"Does the class have other modifications that need to be recorded?"
	traitModification := newDefinition compareAgainstBase: oldDefinition.
	traitModification isEmpty
		ifFalse: [ addition traitsModification addElementModification: traitModification ]
]

{ #category : 'moves' }
RwProjectSetModification >> updateForTraitMoves [
	| traitAdditions traitRemovals |
	traitAdditions := self findAddedTraits.
	traitRemovals := self findRemovedTraits.	"Any keys that match between added and removed should be considered a move."
	traitAdditions
		keysAndValuesDo: [ :key :addition | 
			| removal |
			removal := traitRemovals at: key ifAbsent: [ nil ].
			removal ifNotNil: [ self updateForTraitMoveFrom: removal to: addition ] ]
]
