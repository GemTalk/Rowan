Class {
	#name : 'RwPrjTestTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'smalltalk api' }
RwPrjTestTool >> testSuiteForProjectNamed: projectName [

	| loadedProject suite |
	loadedProject := Rowan image loadedProjectNamed: projectName.
	suite := TestSuite named: projectName , ' tests'.
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
]
