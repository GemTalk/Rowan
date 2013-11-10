"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: initializing - private
"
initializeConflictingPackageNames

	| conflictingPackages |
	conflictingPackages := Dictionary new.
	packageInformationList do: 
			[:each |
			conflictingPackages
				at: each
				put: (knownPackages keys
						select: [:knownName | knownName ~= each name and: [knownName beginsWith: each name , '-']])].
	conflictingPackages := conflictingPackages reject: [:each | each isEmpty].
	conflictingPackages
		keysAndValuesDo: [:package :conflicts | package beConflictedWith: conflicts]