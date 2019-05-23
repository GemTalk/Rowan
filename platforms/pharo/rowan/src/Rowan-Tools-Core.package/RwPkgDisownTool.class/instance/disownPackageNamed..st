packages
disownPackageNamed: packageName

	"Remove the classes and methods managed by the package from the loaded things."

	"The classes and methods are not removed from the system"

	"Use the adopt tool to restore the classes and methods to the loaded things"

	| loadedPackage registry |

	loadedPackage := Rowan image loadedPackageNamed: packageName.
	registry :=Rowan image loadedRegistryForPackageNamed: packageName.

	loadedPackage loadedClasses values
		do: [:loadedClass | 
			| class |
			class := Rowan globalNamed: loadedClass key.
			loadedClass disownFromLoaded: registry.
			registry classRegistry removeKey: class classHistory ].

	loadedPackage loadedClassExtensions values 
		do: [:loadedClassExtension | 
			| class |
			class := Rowan globalNamed: loadedClassExtension key.
			loadedClassExtension disownFromLoaded: registry ].

	registry deletePackage: packageName
