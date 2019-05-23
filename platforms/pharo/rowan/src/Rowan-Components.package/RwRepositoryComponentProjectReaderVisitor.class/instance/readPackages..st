package reading
readPackages: packagesRoot
	packagesRoot directories do: [:packageDir |
		| packageName |
		packageName := self _packageNameFromPackageDir: packageDir.
		(packageDir extension = self packageExtension and: [ self packageNames includes: packageName ])
			ifTrue: [ self readClassesFor: packageName packageRoot: packageDir ] ]