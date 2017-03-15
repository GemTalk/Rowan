"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private - moves
"
findAddedMethods
	"Answer a dictionary of added methods."

	| addedMethods |
	addedMethods := Dictionary new.
	elementsModified do: 
			[:packageModification |
			self addMethodsAddedByPackageModification: packageModification
				toDictionary: addedMethods].
	^addedMethods