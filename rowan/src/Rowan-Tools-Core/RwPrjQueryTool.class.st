Class {
	#name : 'RwPrjQueryTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'smalltalk API' }
RwPrjQueryTool >> classesForProjectNamed: projectName [

	| loadedProject classes |
	classes := IdentitySet new.
	loadedProject := Rowan image loadedProjectNamed: projectName.
	loadedProject loadedPackages
		do: [ :loadedPackage | 
			loadedPackage loadedClasses values
				do: [ :rowanLoadedClass | classes add: rowanLoadedClass handle ] ].
	^ classes
]

{ #category : 'smalltalk API' }
RwPrjQueryTool >> classExtensionsForProjectNamed: projectName [

	| loadedProject classExtensions |
	classExtensions := IdentitySet new.
	loadedProject := Rowan image loadedProjectNamed: projectName.
	loadedProject loadedPackages
		do: [ :loadedPackage | 
			loadedPackage loadedClassExtensions values
				do: [ :rowanLoadedClass | classExtensions add: rowanLoadedClass handle ] ].
	^ classExtensions
]
