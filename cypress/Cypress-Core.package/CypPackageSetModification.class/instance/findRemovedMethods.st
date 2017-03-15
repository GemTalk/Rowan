"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private - moves
"
findRemovedMethods
	"Answer a dictionary of removed methods."

	| removedMethods |
	removedMethods := Dictionary new.
	elementsModified do: 
			[:packageModification |
			self addMethodsRemovedByPackageModification: packageModification
				toDictionary: removedMethods].
	^removedMethods