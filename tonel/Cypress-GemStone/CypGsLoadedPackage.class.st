"
No class-specific documentation for CypGsLoadedPackage, hierarchy is: 
Object
  CypGsLoadedDefinition( properties)
    CypGsLoadedPackage( loadedClasses loadedClassExtensions)

"
Class {
	#name : 'CypGsLoadedPackage',
	#superclass : 'CypLoadedPackage',
	#classVars : [
		'PackageRegistry'
	],
	#category : 'Cypress-GemStone'
}

{ #category : 'private' }
CypGsLoadedPackage class >> allClassesDo: aBlock [
	"Iterate over all classes visible in the current session's transient symbol list, evaluating the block with each."

	GsCurrentSession currentSession symbolList do: 
			[:dict |
			dict do: 
					[:maybeClass |
					(maybeClass isBehavior and: [maybeClass isMeta not])
						ifTrue: [aBlock value: maybeClass]]]
]

{ #category : 'private' }
CypGsLoadedPackage class >> categoryOfClass: aClass [

	^aClass category
]

{ #category : 'queries' }
CypGsLoadedPackage class >> existingOrNewPackageNamed: aName [

	| package |
	package := self existingPackageNamed: aName.
	package == AbsentToken ifTrue: [package := self newNamed: aName].
	^package
]

{ #category : 'queries' }
CypGsLoadedPackage class >> existingPackageNamed: aName [

	^self existingPackageNamed: aName ifAbsent: [AbsentToken]
]

{ #category : 'queries' }
CypGsLoadedPackage class >> existingPackageNamed: aName ifAbsent: absentBlock [

	^PackageRegistry at: aName ifAbsent: absentBlock
]

{ #category : 'initialization' }
CypGsLoadedPackage class >> initialize [
	"CypGsLoadedPackage initialize"

	PackageRegistry := RcKeyValueDictionary new
]

{ #category : 'private' }
CypGsLoadedPackage class >> removeEmptyLoadedPackages [
	"Examine all LoadedPackages. Remove any that have no classes or class extensions."

	PackageRegistry copy
		keysAndValuesDo: [:key :value | value isEmpty ifTrue: [PackageRegistry removeKey: key]]
]

{ #category : 'private' }
CypGsLoadedPackage class >> scanForNewClassesByCategories [
	"If we do not have a LoadedClass for a class in the system, create one and put it in a package
	based on its category."

	CypGsLoadedClass allClassesDo: 
			[:class |
			| loadedClass |
			loadedClass := CypGsLoadedClass existingForClass: class.
			loadedClass == AbsentToken
				ifTrue: 
					[| package |
					package := self existingOrNewPackageNamed: class category asString.
					loadedClass := CypGsLoadedClass newForClass: class.
					package addLoadedClass: loadedClass.
					loadedClass updateFromImageCategory]]
]

{ #category : 'private' }
CypGsLoadedPackage class >> scanForNewMethodsByProtocols [
	"If we do not have a LoadedMethod for a method in the system, create one and put it in a package
	based on its protocol."

	CypGsLoadedMethod allMethodsDo: 
			[:method |
			| loadedMethod |
			loadedMethod := CypGsLoadedMethod existingForMethod: method.
			loadedMethod == AbsentToken
				ifTrue: 
					[| class loadedClass package |
					class := method inClass.
					loadedClass := CypGsLoadedClass existingForClass: class.
					loadedClass == AbsentToken
						ifTrue: [self error: 'Classes should have been scanned before methods.'].
					loadedMethod := CypGsLoadedMethod forMethod: method.
					"Initially, put the method in the package of its class. If this is wrong, updating from protocol will fix it."
					loadedClass addLoadedMethod: loadedMethod.
					loadedMethod updateFromImageProtocols]]
]

{ #category : 'updating' }
CypGsLoadedPackage class >> updateAllFromCategories [
	"Where tools do not yet fully understand Cypress packages, but do handle class categories and method protocols,
	assign each class and method in the system to some LoadedPackage. This will create LoadedPackages as required."
	"Scan the image for classes and methods. Platform-specific"

	self
		updateLoadedClassesFromCategories;
		updateLoadedMethodsFromProtocols;
		scanForNewClassesByCategories;
		scanForNewMethodsByProtocols;
		removeEmptyLoadedPackages
]

{ #category : 'updating' }
CypGsLoadedPackage class >> updateAllFromSymbolDictionaries [
	"Where tools do not yet fully understand Cypress packages, but have been added to SymbolDictionaries and method protocols,
	assign each class and method in the user's SymbolList to some LoadedPackage. This will create LoadedPackages as required."
	"Scan the image for classes and methods. Platform-specific"

	self error: 'Not yet implemented, alas.'
]

{ #category : 'private' }
CypGsLoadedPackage class >> updateLoadedClassesFromCategories [
	"All loadedClasses are in the ClassRegistry. Some of those classes may have been deleted. 
	Try to resolve each by name. Remove the LoadedClasses that are no longer valid. Update the 
	ones whose names now resolve to a different version in the same ClassHistory."

	CypGsLoadedClass
		allDoSafely: [:loadedClass | loadedClass updateFromImageCategory]
]

{ #category : 'private' }
CypGsLoadedPackage class >> updateLoadedMethodsFromProtocols [
	"All loadedMethods are in the ClassRegistry. Some of those classes may have been deleted. 
	Try to resolve each by name. Remove the LoadedClasses that are no longer valid. Update the 
	ones whose names now resolve to a different version in the same ClassHistory."

	CypGsLoadedMethod
		allDoSafely: [:loadedMethod | loadedMethod updateFromImageProtocols]
]

{ #category : 'queries' }
CypGsLoadedPackage >> existingLoadedClassForClass: aClass [
	"If there is an existing loadedClass for the given (non-meta) class in this package, answer it, otherwise the AbsentToken."

	^loadedClasses at: aClass name asString ifAbsent: [AbsentToken]
]

{ #category : 'queries' }
CypGsLoadedPackage >> existingLoadedExtensionForClass: aClass [
	"If there is an existing extension for the given (non-meta) class in this package, answer it, otherwise the AbsentToken."

	^loadedClassExtensions at: aClass classHistory ifAbsent: [AbsentToken]
]

{ #category : 'initialization' }
CypGsLoadedPackage >> initialize [

	super initialize.
	loadedClasses := KeyValueDictionary new.
	loadedClassExtensions := KeyValueDictionary new.
	"A new package is dirty until it has been shown to be identical to a repository."
	isDirty := true
]

{ #category : 'initialization' }
CypGsLoadedPackage >> initializeForName: aName [

	super initializeForName: aName.
	properties at: 'name' put: aName.
	PackageRegistry at: aName put: self
]

{ #category : 'queries' }
CypGsLoadedPackage >> loadedExtensionForClass: aClass [
	"Answer an extension in this package for the given (non-meta) class. Create it if necessary."

	| loadedExtension |
	loadedExtension := self existingLoadedExtensionForClass: aClass.
	loadedExtension == AbsentToken
		ifTrue: 
			["Must create"
			(self existingLoadedClassForClass: aClass) == AbsentToken
				ifFalse: 
					[self
						error: 'Attempt to create a class extension in package that defines the class.'].
			loadedExtension := CypGsLoadedClassExtension newForClass: aClass
						inPackage: self].
	^loadedExtension
]
