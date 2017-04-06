"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: public
"
loadedPackagesNamed: someNames
	"Answer a PackageSet containing packages with the given names in the image.
 	If any name is not a loaded package, that name is ignored."

	| loadedPackages |
	self updateLoadedPackages.
	loadedPackages := someNames
				collect: [:aName | self loadedPackageNamed: aName ifAbsent: [nil]].
	^CypPackageSet withAll: (loadedPackages select: [:each | each notNil])