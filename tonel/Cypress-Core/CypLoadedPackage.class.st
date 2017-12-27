"
repository: The repository I was loaded from (if known, nil otherwise)
commitId: A repository-specific way of identifying source code commit, if it's a type of repository that has that kind of information (nil otherwise)
loadedClasses: Dictionary -- keys are (non-meta) classes, values are the LoadedClasses that I define.
loadedClassExtensions: -- Dictionary -- keys are (non-meta) classes, values are the LoadedClassExtensions that I define.
"
Class {
	#name : 'CypLoadedPackage',
	#superclass : 'CypLoadedThing',
	#instVars : [
		'repository',
		'commitId',
		'loadedClasses',
		'loadedClassExtensions',
		'isDirty'
	],
	#category : 'Cypress-Core'
}

{ #category : 'accessing' }
CypLoadedPackage >> addLoadedClass: aLoadedClass [

	self markDirty.
	loadedClasses at: aLoadedClass key put: aLoadedClass.
	aLoadedClass loadedPackage: self
]

{ #category : 'accessing' }
CypLoadedPackage >> addLoadedClassExtension: aLoadedClassExtension [

	self markDirty.
	loadedClassExtensions at: aLoadedClassExtension key
		put: aLoadedClassExtension.
	aLoadedClassExtension loadedPackage: self
]

{ #category : 'definitions' }
CypLoadedPackage >> asDefinition [
	"Create a Definition corresponding to what I define in the image."

	^CypPackageDefinition
		withProperties: self propertiesForDefinition
		classDefinitions: self classDefinitions
		classExtensions: self classExtensionDefinitions
]

{ #category : 'private' }
CypLoadedPackage >> classDefinitions [
	"Create definitions from all of the classes I define, and answer the collection of them"

	^self definitionsFor: loadedClasses
]

{ #category : 'private' }
CypLoadedPackage >> classExtensionDefinitions [
	"Create definitions from all of the class extensions I define, and answer the collection of them"

	^self definitionsFor: loadedClassExtensions
]

{ #category : 'queries' }
CypLoadedPackage >> classOrExtensionForClassNamed: className ifAbsent: absentBlock [

	^loadedClasses at: className
		ifAbsent: [loadedClassExtensions at: className ifAbsent: absentBlock]
]

{ #category : 'testing' }
CypLoadedPackage >> isEmpty [

	^loadedClasses isEmpty and: [loadedClassExtensions isEmpty]
]

{ #category : 'accessing' }
CypLoadedPackage >> loadedClasses [
	^loadedClasses

]

{ #category : 'accessing' }
CypLoadedPackage >> loadedClasses: newValue [
	loadedClasses := newValue

]

{ #category : 'accessing' }
CypLoadedPackage >> loadedClassExtensions [
	^loadedClassExtensions

]

{ #category : 'accessing' }
CypLoadedPackage >> loadedClassExtensions: newValue [
	loadedClassExtensions := newValue

]

{ #category : 'private' }
CypLoadedPackage >> markDirty [
	"I have been changed independent of a change to my repository, and therefore may be out of sync with my repository"

	isDirty := true
]

{ #category : 'private' }
CypLoadedPackage >> markNotDirty [
	"I have been changed to be consistent with my source, so mark not dirty."

	isDirty := false
]

{ #category : 'private' }
CypLoadedPackage >> markPackageDirty [

	self markDirty
]

{ #category : 'accessing' }
CypLoadedPackage >> removeLoadedClass: aLoadedClass [

	self markDirty.
	loadedClasses removeKey: aLoadedClass key
]
