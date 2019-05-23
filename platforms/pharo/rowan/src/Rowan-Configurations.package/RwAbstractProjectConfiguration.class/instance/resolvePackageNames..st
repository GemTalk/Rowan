load support
resolvePackageNames: visitedConfigurationNames

	"return the list of package names that is the result of resolving all of the receiver's references.
		Do not visit configurations already listed in visitedConfigurationNames "

	self subclassResponsibility: #'resolvePackageNames:'