private
_loadedPackageDefinitionsNamed: someNames

	"Answer a PackageSetDefinition containing packages with the given names in the image.
 	If any name is not a loaded package, that name is ignored."

	^ (self _loadedPackagesNamed: someNames) asPackageDefinitionSet
