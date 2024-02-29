Extension { #name : 'RwGsImage' }

{ #category : '*rowan-gemstone-loader-extensions-onlyv2' }
RwGsImage class >> _cloneRowanLoaderSymbolDictionary [

	"in order to cleanly update Rowan using Rowan, it is necessary to isolate a copy of all of the loader code
		in a separate symbol dictionary and then use the isolated copy to execute the update."

	| rowanLoaderSymbolDict clonedSymDictName clonedSymDict |
	rowanLoaderSymbolDict := Rowan image symbolDictNamed: 'RowanLoader'.
	clonedSymDictName := #'RowanLoader_cloned'.
	clonedSymDict := rowanLoaderSymbolDict 
		_rowanCloneSymbolDictionaryNamed: clonedSymDictName 
		symbolList: GsCurrentSession currentSession symbolList.
	Transcript cr; show: 'RowanLoader symbol dictionary cloned'.
	^ clonedSymDict
]

{ #category : '*rowan-gemstone-loader-extensions-onlyv2' }
RwGsImage class >> _shouldCloneRowanLoader: aProjectSetModification [
	"When modifications are made to the Rowan-GemStone-Loader package in the Rowan project, we need to clone the RowanLoader symbol dictionary"

	| projectModification |
	projectModification := aProjectSetModification elementsModified
		at: 'Rowan'
		ifAbsent: [ ^ false ].
	#('Rowan-GemStone-LoaderV2')
		do: [ :loaderPackageName | 
			(projectModification packagesModification elementsModified
				at: loaderPackageName
				ifAbsent: [  ])
				ifNotNil: [ :packageModification | ^ packageModification isEmpty not ] ].
	^ false
]

{ #category : '*rowan-gemstone-loader-extensions-onlyv2' }
RwGsImage class >> applyModification_V2: aProjectSetModification instanceMigrator: instanceMigrator symbolList: symbolList [
	(self _shouldCloneRowanLoader: aProjectSetModification)
		ifTrue: [ 
			self
				applyModification_V2: aProjectSetModification
				visitorClass:
					(self _cloneRowanLoaderSymbolDictionary at: RwGsImagePatchVisitor_V2 name)
				instanceMigrator: instanceMigrator
				symbolList: symbolList ]
		ifFalse: [ 
			self
				applyModification_V2: aProjectSetModification
				visitorClass: RwGsImagePatchVisitor_V2
				instanceMigrator: instanceMigrator
				symbolList: symbolList ]
]

{ #category : '*rowan-gemstone-loader-extensions-onlyv2' }
RwGsImage class >> applyModification_V2: aProjectSetModification visitorClass: visitorClass instanceMigrator: instanceMigrator symbolList: symbolList [
	| visitor patchSet newClassVersionPatchSet |
	visitor := visitorClass new.
	visitor visit: aProjectSetModification.
	patchSet := visitor patchSet.
	patchSet loadSymbolList: symbolList.
	patchSet classesWithNewVersions isEmpty
		ifTrue: [ 
			patchSet apply.
			^ self ].
	patchSet setupForNewClassVersionUpdates.
	patchSet classesWithNewVersions
		do: [ :each | 
			patchSet
				updatePatchesForNewClassVersion: each
				projectModification: aProjectSetModification ].
	visitor := visitorClass new.
	visitor visit: aProjectSetModification.
	newClassVersionPatchSet := visitor patchSet.
	newClassVersionPatchSet applyForNewClassVersions: instanceMigrator
]

{ #category : '*rowan-gemstone-loader-extensions-onlyv2' }
RwGsImage class >> loadedClassAndSymbolDicitonaryForClass: class ifPresent: presentBlock ifAbsent: absentBlock [
	"scan the symbol list for a loaded package that matches the loaded package for the given loaded class"

	| loadedClass loadedPackage packageName |
	loadedClass := self symbolDictionaryRegistryClass registry_ImplementationClass loadedClassForClass: class.
	loadedPackage := loadedClass loadedPackage.
	packageName := loadedPackage name.
	self symbolList
		do: [ :symbolDict | 
			symbolDict rowanSymbolDictionaryRegistry
				ifNotNil: [ :registry | 
					(registry loadedPackageNamed: packageName ifAbsent: [  ])
						ifNotNil: [ :lp | 
							lp == loadedPackage 
								ifTrue: [ ^ presentBlock value: (symbolDict) value: loadedClass ] ] ] ].
	^ absentBlock value
]

{ #category : '*rowan-gemstone-loader-extensions-onlyv2' }
RwGsImage class >> loadedClassExtensionsForClass: class [
	"lookup the loadedClassExtensions for the given class"

	^ self symbolDictionaryRegistryClass registry_ImplementationClass loadedClassExtensionsForClass: class ifAbsent: [ IdentitySet new ]
]

{ #category : '*rowan-gemstone-loader-extensions-onlyv2' }
RwGsImage class >> loadedClassForClass: class ifAbsent: absentBlock [
	"Lookup the given class in the classRegistry"

	^ self symbolDictionaryRegistryClass registry_ImplementationClass 
		loadedClassForClass: class
		ifAbsent: absentBlock
]

{ #category : '*rowan-gemstone-loader-extensions-onlyv2' }
RwGsImage class >> loadedClassNamed: className ifFound: foundBlock ifAbsent: absentBlock [

	"scan the symbol list for a RwLoadedClass instance of the given name"

	| class loadedClass |
	class := self objectNamed: className.
	class ifNil: [ ^ absentBlock value ].
	loadedClass := self symbolDictionaryRegistryClass registry_ImplementationClass 
		loadedClassForClass: class
		ifAbsent: [ ^ absentBlock value ].
	^ foundBlock value: loadedClass
]

{ #category : '*rowan-gemstone-loader-extensions-onlyv2' }
RwGsImage class >> removeLoadedClassExtensionsForClass: class [
	"The class has or will be deleted from the system, remove the loadedClassExtensions that refer
		to the given class"

	(self symbolDictionaryRegistryClass registry_ImplementationClass
		loadedClassExtensionsForClass: class
		ifAbsent: [ #() ]) copy
		do: [ :loadedClassExtension | 
			self symbolDictionaryRegistryClass registry_ImplementationClass
				unregisterLoadedClassExtension: loadedClassExtension
				forClass: class ]
]

{ #category : '*rowan-gemstone-loader-extensions-onlyv2' }
RwGsImage class >> symbolDictionaryRegistryClass [

	^ RwGsSymbolDictionaryRegistryV2
]
