private
_addTestsForProjectNamed: projectName toTestSuite: suite

	| loadedProject |
	loadedProject := Rowan image loadedProjectNamed: projectName.
	loadedProject loadedPackages
		do: [ :loadedPackage | 
			loadedPackage loadedClasses values
				do: [ :rowanLoadedClass | 
					| cl |
					cl := rowanLoadedClass handle.
					(cl inheritsFrom: TestCase)
						ifTrue: [ 
							cl isAbstract
								ifFalse: [ suite addTests: cl suite tests ] ] ] ].
	^ suite
