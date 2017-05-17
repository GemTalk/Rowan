"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private-installing
"
loadedClassOrExtensionForMethod
	"The loaded class or extension should already exist."

	| packageName loadedPackage className |
	packageName := packageDefinition key.
	loadedPackage := Cypress loadedPackageNamed: packageName
				ifAbsent: 
					[self
						error: 'Internal error -- attempt to add a method to a nonexistent package.'].
	className := classDefinition key.
	^loadedPackage classOrExtensionForClassNamed: className
		ifAbsent: 
			[self
				error: 'Internal error -- attempt to add a method to a package in which its class is neither defined nor extended.']