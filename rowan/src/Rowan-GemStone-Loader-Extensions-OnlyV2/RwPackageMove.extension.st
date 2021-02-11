Extension { #name : 'RwPackageMove' }

{ #category : '*rowan-gemstone-loader-extensions-onlyv2' }
RwPackageMove >> addMovedPackageToPatchSet: aPatchSet [

	aPatchSet addPackageMove: self
]

{ #category : '*rowan-gemstone-loader-extensions-onlyv2' }
RwPackageMove >> movePackage: classesWithNewVersions [
	"Loader already makes sure that the package is created properly, but we do have to 
		make sure that the old package is deleted from it's original location. Class extensions
		have to be removed as well (https://github.com/dalehenrich/Rowan/issues/495). 
		See also https://github.com/dalehenrich/Rowan/issues/680"

self halt. "this method is called when a package is being moved between projects -- projectAfter and projectBefore can be used to confirm ... so it seems that we just need to move the loaded package from one project to another and Bob's your uncle...
"
"
	(Rowan image loadedRegistryForPackageNamed: self packageDefinition name)
		movePackage: self packageDefinition name
		to: self packageDefinition gs_symbolDictionary
		classesWithNewVersions: classesWithNewVersions
"
]
