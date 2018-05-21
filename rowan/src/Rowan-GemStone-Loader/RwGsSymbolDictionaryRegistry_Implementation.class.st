"
The Rowan-GemStone-Loader implementation does depend upon having a stable and consistent implementation of the RwGsSymbolDictionaryRegistry behavior for the duration of a load.

Right before a load, all of the classes that make up the Rowan-GemStone-Loader package are copied and isolated from the rest of the system, so that updates to the loader code itself will not change the behavior of the loader implementation while the load is in progress.

The class RwGsSymbolDictionaryRegistry is not part of the Rowan-GemStone-Loader package, because instance of the class are expected to have long lifetimes.

This class implements the behavior for RwGsSymbolDictionaryRegistry and since it is the Rowan-GemStone-Loader package, it can be copied and isolated during a load, without affecting the long lived instances of RwGsSymbolDictionaryRegistry.
"
Class {
	#name : 'RwGsSymbolDictionaryRegistry_Implementation',
	#superclass : 'Object',
	#category : 'Rowan-GemStone-Loader'
}

{ #category : 'private' }
RwGsSymbolDictionaryRegistry_Implementation class >> addClassAssociation: assoc forClass: class toPackageNamed: packageName instance: registryInstance [

	"Copy the name association to the correct 
        SymbolDictionary in the live SymbolList.
        Create a LoadedClass for the new class, add it to the defining LoadedPackage."

	| loadedPackage loadedClass |
	loadedPackage := registryInstance existingOrNewLoadedPackageNamed: packageName.

	registryInstance classRegistry
		at: class classHistory
		ifAbsent: [ 
			loadedClass := RwGsLoadedSymbolDictClass newForClass: class.
			loadedPackage addLoadedClass: loadedClass.
			loadedClass updatePropertiesFromClassFor: self.
			registryInstance classRegistry at: class classHistory put: loadedClass.
			(loadedPackage loadedClassExtensions at: class name ifAbsent: [  ])
				ifNotNil: [ :loadedClassExtension | 
					"I think we need to subsume the loadedClassExtension methods into a loadedClass ..."
					"have yet to come with a test case that takes this path"
					self error: 'internal error - unexpected class extenstions for a loaded class ', class name asString printString , ' that has no entry in classRegistry in package ', loadedPackage name printString, '.' ] ].

	registryInstance _symbolDictionaryAt: assoc key 
		ifPresent: [
			"If there is an existing association, then we need to use the existing association which may have references
				in compiled methods --- bootstrapping Rowan in Rowan is only valid use case"
			(registryInstance _symbolDictionary associationAt: assoc key) value: class ]
		ifAbsent: [
			assoc value: class.
			registryInstance _symbolDictionary add: assoc]

]
