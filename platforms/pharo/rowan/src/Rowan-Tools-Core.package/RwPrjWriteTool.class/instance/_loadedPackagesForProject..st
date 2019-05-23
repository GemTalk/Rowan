private
_loadedPackagesForProject: projectDefinition

	"Answer a PackageSet containing packages for the given project definition."

	^ self
		_loadedPackagesNamed: projectDefinition packageNames
		forProject: projectDefinition
