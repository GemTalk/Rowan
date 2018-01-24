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

{ #category : 'definitions' }
RwLoadedClassExtension >> asDefinition [
	"Create a Definition corresponding to what I define in the image."
	^RwClassExtensionDefinition
		withProperties: self propertiesForDefinition
		classMethodDefinitions: self classMethodDefinitions
		instanceMethodDefinitions: self instanceMethodDefinitions
]

{ #category : 'private' }
RwLoadedClassExtension >> classMethodDefinitions [

	^self definitionsFor: loadedClassMethods
]

{ #category : 'private' }
RwLoadedClassExtension >> instanceMethodDefinitions [

	^self definitionsFor: loadedInstanceMethods
]

{ #category : 'testing' }
RwLoadedClassExtension >> isEmpty [

	^loadedInstanceMethods isEmpty and: [loadedClassMethods isEmpty]
]

{ #category : 'accessing' }
RwLoadedClassExtension >> loadedPackage [

	^loadedPackage
]

{ #category : 'accessing' }
RwLoadedClassExtension >> loadedPackage: aLoadedPackage [

	loadedPackage := aLoadedPackage
]

{ #category : 'private' }
RwLoadedClassExtension >> markPackageDirty [

	loadedPackage markDirty
]

{ #category : 'accessing' }
RwLoadedClassExtension >> packageName [

	^loadedPackage name
]
