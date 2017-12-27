"
package: the CypLoadedPackage that defines me and all my methods that are not extension methods.
extensions: a collection of CypLoadedExtendedClasses for this class. From these we can tell which methods don't belong to our default package.
"
Class {
	#name : 'CypLoadedClass',
	#superclass : 'CypLoadedThing',
	#instVars : [
		'loadedPackage',
		'loadedInstanceMethods',
		'loadedClassMethods'
	],
	#category : 'Cypress-Core'
}

{ #category : 'definitions' }
CypLoadedClass >> asDefinition [
	"Create a Definition corresponding to what I define in the image."
	^CypClassDefinition
		withProperties: self propertiesForDefinition
		classMethodDefinitions: self classMethodDefinitions
		instanceMethodDefinitions: self instanceMethodDefinitions
]

{ #category : 'private' }
CypLoadedClass >> classMethodDefinitions [

	^self definitionsFor: loadedClassMethods
]

{ #category : 'private' }
CypLoadedClass >> instanceMethodDefinitions [

	^self definitionsFor: loadedInstanceMethods
]

{ #category : 'accessing' }
CypLoadedClass >> key [
	"Answer some token that will uniquely identify me relative to any other LoadedClass in the same package."

	self subclassResponsibility
]

{ #category : 'accessing' }
CypLoadedClass >> loadedPackage [

	^loadedPackage
]

{ #category : 'accessing' }
CypLoadedClass >> loadedPackage: aLoadedPackage [

	loadedPackage := aLoadedPackage
]

{ #category : 'private' }
CypLoadedClass >> markPackageDirty [

	loadedPackage markDirty
]

{ #category : 'accessing' }
CypLoadedClass >> packageName [

	^loadedPackage name
]
