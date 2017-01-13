"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: public
"
loadedPackagesNamed: someNames
	"Answer a PackageSet containing packages with the given names in the image.
 	If any name is not a loaded package, a new LoadedPackage is created and answered as part of the set.
	It will be empty and have default properties."

	| packageRefs |
	self updateLoadedPackages.
	packageRefs := someNames
				collect: [:aName | self existingOrNewLoadedPackageNamed: aName].
	^CypPackageSet withAll: packageRefs