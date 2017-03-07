"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: moves
"
findMethodMoves

	| addedMethods removedMethods |
	addedMethods := Dictionary new.
	removedMethods := Dictionary new.
	elementsAdded
		do: [:newPackage | self addMethodsDefinedInPackage: newPackage to: addedMethods].
	elementsRemoved
		do: [:oldPackage | self addMethodsDefinedInPackage: oldPackage to: removedMethods].
	elementsModified do: 
			[:packageModification |
			self addMethodsAddedByPackageModification: packageModification
				toDictionary: addedMethods.
			self addMethodsRemovedByPackageModification: packageModification
				toDictionary: removedMethods].


	"Now have all methods added or removed in the package set modification.
	Any keys that match between added and removed should be considered a move."
	addedMethods keysAndValuesDo: 
			[:key :added |
			| removed |
			removed := removedMethods at: key ifAbsent: [nil].
			removed
				ifNotNil: 
					[movedMethods add: (CypMethodMove
								methodBefore: removed value
								methodAfter: added value
								classOrExtensionBefore: removed key value
								classOrExtensionAfter: added key value
								packageBefore: removed key key
								packageAfter: added key key
								isMeta: key key value)]]