Class {
	#name : 'RwPkgQueryTool',
	#superclass : 'RwPackageTool',
	#category : 'Rowan-Tools-Deprecated'
}

{ #category : 'smalltalk API' }
RwPkgQueryTool >> classesForPackageNamed: projectName [

	| loadedPackage classes |
	classes := IdentitySet new.
	loadedPackage := Rowan image loadedPackageNamed: projectName.
	loadedPackage loadedClasses values
		do: [ :rowanLoadedClass | classes add: rowanLoadedClass handle ].
	^ classes
]

{ #category : 'smalltalk API' }
RwPkgQueryTool >> classExtensionsForPackageNamed: projectName [

	| loadedPackage classExtensions |
	classExtensions := IdentitySet new.
	loadedPackage := Rowan image loadedPackageNamed: projectName.
	loadedPackage loadedClassExtensions values
		do: [ :rowanLoadedClass | classExtensions add: rowanLoadedClass handle ].
	^ classExtensions
]
