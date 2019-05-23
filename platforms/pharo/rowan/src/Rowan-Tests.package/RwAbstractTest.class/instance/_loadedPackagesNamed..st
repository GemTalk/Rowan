private
_loadedPackagesNamed: someNames

	"Answer a PackageSet containing packages with the given names in the image.
 	If any name is not a loaded package, that name is ignored."

	"very likely we should be dealing with a project name 
at this point ... unless there is a use case for loading a collection of packages without directly using the project interface --- loaded packages do know what project they belong to ... so there might be something done behind the scenes"

	| loadedPackages |
	loadedPackages := someNames
		collect: [ :aName | Rowan image loadedPackageNamed: aName ifAbsent: [ nil ] ].
	^ RwEntitySet withAll: (loadedPackages select: [ :each | each notNil ])
