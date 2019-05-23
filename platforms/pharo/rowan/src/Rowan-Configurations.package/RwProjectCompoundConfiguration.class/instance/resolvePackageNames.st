load support
resolvePackageNames

	"return the list of package names that is the result of resolving all of the receiver's references"

	"resolve each of the configurations in configurationNames to a list of package names and combine
		with the receiver's package names"

	| visitedConfigurationNames |
	visitedConfigurationNames := Set new.
	^ self resolvePackageNames: visitedConfigurationNames