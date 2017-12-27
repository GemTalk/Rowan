"
No class-specific documentation for CypLoadedClassExtension, hierarchy is: 
Object
  CypLoadedThing( properties name handle)
    CypLoadedClassExtension( loadedPackage loadedMethods)

"
Class {
	#name : 'CypLoadedClassExtension',
	#superclass : 'CypLoadedThing',
	#instVars : [
		'loadedPackage',
		'loadedInstanceMethods',
		'loadedClassMethods'
	],
	#category : 'Cypress-Core'
}

{ #category : 'accessing' }
CypLoadedClassExtension >> loadedPackage: aLoadedPackage [

	loadedPackage := aLoadedPackage
]

{ #category : 'private' }
CypLoadedClassExtension >> markPackageDirty [

	loadedPackage markDirty
]
