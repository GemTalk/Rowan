"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: initializing - private
"
initializeQualifiedPackageNames

	| qualifiedPackages |
	qualifiedPackages := Dictionary new.
	packageInformationList do: 
			[:each |
			qualifiedPackages at: each
				put: (knownPackages keys select: 
							[:knownName |
							knownName ~= each name
								and: [(each name indexOfSubCollection: knownName , '-') = 1]])].
	qualifiedPackages := qualifiedPackages reject: [:each | each isEmpty].
	qualifiedPackages
		keysAndValuesDo: [:package :baseNames | package beQualifiedNameOf: baseNames]
