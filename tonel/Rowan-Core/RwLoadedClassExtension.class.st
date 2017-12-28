"
No class-specific documentation for CypLoadedClassExtension, hierarchy is: 
Object
  CypLoadedThing( properties name handle)
    CypLoadedClassExtension( loadedPackage loadedMethods)

"
Class {
	#name : 'RwLoadedClassExtension',
	#superclass : 'RwLoadedThing',
	#instVars : [
		'loadedPackage',
		'loadedInstanceMethods',
		'loadedClassMethods'
	],
	#category : 'Rowan-Core'
}

{ #category : 'accessing' }
RwLoadedClassExtension >> loadedPackage: aLoadedPackage [

	loadedPackage := aLoadedPackage
]

{ #category : 'private' }
RwLoadedClassExtension >> markPackageDirty [

	loadedPackage markDirty
]
