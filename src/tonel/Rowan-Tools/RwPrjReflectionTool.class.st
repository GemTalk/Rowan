Class {
	#name : 'RwPrjReflectionTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools'
}

{ #category : 'smalltalk API' }
RwPrjReflectionTool >> classesForProjectNamed: projectName [

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
RwPrjReflectionTool >> classExtensionsForProjectNamed: projectName [

	| loadedProject classExtensions |
	classExtensions := IdentitySet new.
	loadedProject := Rowan image loadedProjectNamed: projectName.
	loadedProject loadedPackages
		do: [ :loadedPackage | 
			loadedPackage loadedClassExtensions values
				do: [ :rowanLoadedClass | classExtensions add: rowanLoadedClass handle ] ].
	^ classExtensions
]
