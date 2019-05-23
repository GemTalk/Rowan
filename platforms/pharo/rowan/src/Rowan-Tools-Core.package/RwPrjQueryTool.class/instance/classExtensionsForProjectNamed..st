smalltalk API
classExtensionsForProjectNamed: projectName

	| loadedProject classExtensions |
	classExtensions := IdentitySet new.
	loadedProject := Rowan image loadedProjectNamed: projectName.
	loadedProject loadedPackages
		do: [ :loadedPackage | 
			loadedPackage loadedClassExtensions values
				do: [ :rowanLoadedClass | classExtensions add: rowanLoadedClass handle ] ].
	^ classExtensions