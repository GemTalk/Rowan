"
No class-specific documentation for CypGsLoadedPackage, hierarchy is: 
Object
  CypGsLoadedDefinition( properties)
    CypGsLoadedPackage( loadedClasses loadedClassExtensions)

"
Class {
	#name : 'RwGsLoadedPackage',
	#superclass : 'RwLoadedPackage',
	#classVars : [
		'PackageRegistry'
	],
	#category : 'Rowan-GemStone'
}

{ #category : 'private' }
RwGsLoadedPackage class >> allClassesDo: aBlock [

	"Iterate over all classes visible in the current session's transient symbol list, evaluating the block with each."

	Rowan image symbolList
		do: [ :dict | 
			dict
				do: [ :maybeClass | 
					(maybeClass isBehavior and: [ maybeClass isMeta not ])
						ifTrue: [ aBlock value: maybeClass ] ] ]
]

{ #category : 'private' }
RwGsLoadedPackage class >> categoryOfClass: aClass [

	^aClass category
]

{ #category : 'queries' }
RwGsLoadedPackage class >> existingOrNewPackageNamed: aName [

	| package |
	package := self existingPackageNamed: aName.
	package == AbsentToken ifTrue: [package := self newNamed: aName].
	^package
]

{ #category : 'queries' }
RwGsLoadedPackage class >> existingPackageNamed: aName [

	^self existingPackageNamed: aName ifAbsent: [AbsentToken]
]

{ #category : 'queries' }
RwGsLoadedPackage class >> existingPackageNamed: aName ifAbsent: absentBlock [

	^PackageRegistry at: aName ifAbsent: absentBlock
]

{ #category : 'initialization' }
RwGsLoadedPackage class >> initialize [
	"RwGsLoadedPackage initialize"

	PackageRegistry := RcKeyValueDictionary new
]

{ #category : 'private' }
RwGsLoadedPackage class >> removeEmptyLoadedPackages [
	"Examine all LoadedPackages. Remove any that have no classes or class extensions."

	PackageRegistry copy
		keysAndValuesDo: [:key :value | value isEmpty ifTrue: [PackageRegistry removeKey: key]]
]

{ #category : 'private' }
RwGsLoadedPackage class >> scanForNewClassesByCategories [
	"If we do not have a LoadedClass for a class in the system, create one and put it in a package
	based on its category."

	RwGsLoadedClass allClassesDo:
			[:class |
			| loadedClass |
			loadedClass := RwGsLoadedClass existingForClass: class.
			loadedClass == AbsentToken
				ifTrue: 
					[| package |
					package := self existingOrNewPackageNamed: class category asString.
					loadedClass := RwGsLoadedClass newForClass: class.
					package addLoadedClass: loadedClass.
					loadedClass updateFromImageCategory]]
]

{ #category : 'private' }
RwGsLoadedPackage class >> scanForNewMethodsByProtocols [

	"If we do not have a LoadedMethod for a method in the system, create one and put it in a package
	based on its protocol."

	RwGsLoadedMethod
		allMethodsDo: [ :method | 
			| loadedMethod |
			loadedMethod := RwGsLoadedMethod existingForMethod: method.
			loadedMethod == AbsentToken
				ifTrue: [ 
					| class loadedClass |
					class := method inClass.
					loadedClass := RwGsLoadedClass existingForClass: class.
					loadedClass == AbsentToken
						ifTrue: [ self error: 'Classes should have been scanned before methods.' ].
					loadedMethod := RwGsLoadedMethod forMethod: method.	"Initially, put the method in the package of its class. If this is wrong, updating from protocol will fix it."
					loadedClass addLoadedMethod: loadedMethod.
					loadedMethod updateFromImageProtocols ] ]
]

{ #category : 'updating' }
RwGsLoadedPackage class >> updateAllFromCategories [
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
RwGsLoadedPackage class >> updateAllFromSymbolDictionaries [
	"Where tools do not yet fully understand Cypress packages, but have been added to SymbolDictionaries and method protocols,
	assign each class and method in the user's SymbolList to some LoadedPackage. This will create LoadedPackages as required."
	"Scan the image for classes and methods. Platform-specific"

	self error: 'Not yet implemented, alas.'
]

{ #category : 'private' }
RwGsLoadedPackage class >> updateLoadedClassesFromCategories [
	"All loadedClasses are in the ClassRegistry. Some of those classes may have been deleted. 
	Try to resolve each by name. Remove the LoadedClasses that are no longer valid. Update the 
	ones whose names now resolve to a different version in the same ClassHistory."

	RwGsLoadedClass
		allDoSafely: [:loadedClass | loadedClass updateFromImageCategory]
]

{ #category : 'private' }
RwGsLoadedPackage class >> updateLoadedMethodsFromProtocols [
	"All loadedMethods are in the ClassRegistry. Some of those classes may have been deleted. 
	Try to resolve each by name. Remove the LoadedClasses that are no longer valid. Update the 
	ones whose names now resolve to a different version in the same ClassHistory."

	RwGsLoadedMethod
		allDoSafely: [:loadedMethod | loadedMethod updateFromImageProtocols]
]

{ #category : 'queries' }
RwGsLoadedPackage >> existingLoadedClassForClass: aClass [
	"If there is an existing loadedClass for the given (non-meta) class in this package, answer it, otherwise the AbsentToken."

	^loadedClasses at: aClass name asString ifAbsent: [AbsentToken]
]

{ #category : 'queries' }
RwGsLoadedPackage >> existingLoadedExtensionForClass: aClass [
	"If there is an existing extension for the given (non-meta) class in this package, answer it, otherwise the AbsentToken."

	^loadedClassExtensions at: aClass classHistory ifAbsent: [AbsentToken]
]

{ #category : 'initialization' }
RwGsLoadedPackage >> initialize [

	super initialize.
	loadedClasses := KeyValueDictionary new.
	loadedClassExtensions := KeyValueDictionary new.
	"A new package is dirty until it has been shown to be identical to a repository."
	isDirty := true
]

{ #category : 'initialization' }
RwGsLoadedPackage >> initializeForName: aName [

	super initializeForName: aName.
	properties at: 'name' put: aName.
	PackageRegistry at: aName put: self
]

{ #category : 'queries' }
RwGsLoadedPackage >> loadedExtensionForClass: aClass [
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
			loadedExtension := RwGsLoadedClassExtension newForClass: aClass
						inPackage: self].
	^loadedExtension
]

{ #category : 'removing' }
RwGsLoadedPackage >> removeFromSystem [

	self isEmpty 
		ifFalse: [ 
			 "This may not be an error, but in that case, we'll need to delete the contents of the 				package from the system and at this point that seems to be a bit late in the process, 
				since package removal is the last thing done..."
			self error: 'Internal Error - attempt to remove an non-empty package from system' ].
	PackageRegistry removeKey: self name
]
