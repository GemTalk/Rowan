smalltalk API
classesForProjectNamed: projectName

	| loadedProject classes |
	classes := IdentitySet new.
	loadedProject := Rowan image loadedProjectNamed: projectName.
	loadedProject loadedPackages
		do: [ :loadedPackage | 
			loadedPackage loadedClasses values
				do: [ :rowanLoadedClass | classes add: rowanLoadedClass handle ] ].
	^ classes