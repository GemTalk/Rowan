"
notice: This work is protected by copyright. All rights reserved.
category: initializing
"
initializeQualifiedPackageNames

	| qualifiedPackages |
	qualifiedPackages := Dictionary new.
	packageInformationList do: 
			[:each |
			qualifiedPackages
				at: each
				put: (knownPackages keys
						select: [:knownName | knownName ~= each name and: [each name beginsWith: knownName , '-']])].
	qualifiedPackages := qualifiedPackages reject: [:each | each isEmpty].
	qualifiedPackages
		keysAndValuesDo: [:package :baseNames | package beQualifiedNameOf: baseNames]