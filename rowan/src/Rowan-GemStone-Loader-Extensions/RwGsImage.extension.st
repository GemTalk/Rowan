Extension { #name : 'RwGsImage' }

{ #category : '*rowan-gemstone-loader-extensions' }
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

{ #category : '*rowan-gemstone-loader-extensions' }
RwGsImage class >> _shouldCloneRowanLoader: aProjectSetModification [

	"When modifications are made to the Rowan-GemStone-Loader package in the Rowan project, we need to clone the RowanLoader symbol dictionary"

	| projectModification packageModification |
	projectModification := aProjectSetModification elementsModified at: 'Rowan' ifAbsent: [ ^ false ].
	packageModification := projectModification packagesModification elementsModified at: 'Rowan-GemStone-Loader' ifAbsent: [ ^ false ].
	^ packageModification isEmpty not

]

{ #category : '*rowan-gemstone-loader-extensions' }
RwGsImage class >> applyModification_254: aProjectSetModification instanceMigrator: instanceMigrator [
	(self _shouldCloneRowanLoader: aProjectSetModification)
		ifTrue: [ 
			| visitorClass |
			visitorClass := self _cloneRowanLoaderSymbolDictionary at: #RwGsImagePatchVisitor_254.
			self 
				applyModification_254: aProjectSetModification 
				visitorClass:visitorClass 
				instanceMigrator: instanceMigrator ]
		ifFalse: [ 
			self 
				applyModification_254: aProjectSetModification 
				visitorClass: RwGsImagePatchVisitor_254 
				instanceMigrator: instanceMigrator ]

]

{ #category : '*rowan-gemstone-loader-extensions' }
RwGsImage class >> applyModification_254: aProjectSetModification visitorClass: visitorClass instanceMigrator: instanceMigrator [

	| visitor patchSet newClassVersionPatchSet |
	visitor := visitorClass new.
	visitor visit: aProjectSetModification.
	patchSet := visitor patchSet.
	patchSet classesWithNewVersions isEmpty
		ifTrue: [ 
			patchSet apply.
			^ self ].
	patchSet setupForNewClassVersionUpdates.
	patchSet classesWithNewVersions
		do: [ :each | 
			each
				updatePatchesForNewClassVersion: aProjectSetModification 
					patchSet: patchSet;
				updateNewClassVersionPatchesForExtensionsIn: aProjectSetModification
					patchSet: patchSet;
				updateNewClassVersionPatchesForSubclassesIn: aProjectSetModification
					patchSet: patchSet ].
	visitor := visitorClass new.
	visitor visit: aProjectSetModification.
	newClassVersionPatchSet := visitor patchSet.
	newClassVersionPatchSet applyForNewClassVersions: instanceMigrator

]

{ #category : '*rowan-gemstone-loader-extensions' }
RwGsImage class >> applyModification_V2: aProjectSetModification instanceMigrator: instanceMigrator [
	(self _shouldCloneRowanLoader: aProjectSetModification)
		ifTrue: [ 
			| visitorClass |
			visitorClass := self _cloneRowanLoaderSymbolDictionary at: #RwGsImagePatchVisitor_V2.
			self 
				applyModification_V2: aProjectSetModification 
				visitorClass:visitorClass 
				instanceMigrator: instanceMigrator ]
		ifFalse: [ 
			self 
				applyModification_V2: aProjectSetModification 
				visitorClass: RwGsImagePatchVisitor_V2 
				instanceMigrator: instanceMigrator ]
]

{ #category : '*rowan-gemstone-loader-extensions' }
RwGsImage class >> applyModification_V2: aProjectSetModification visitorClass: visitorClass instanceMigrator: instanceMigrator [

	| visitor patchSet newClassVersionPatchSet |
	visitor := visitorClass new.
	visitor visit: aProjectSetModification.
	patchSet := visitor patchSet.
	patchSet classesWithNewVersions isEmpty
		ifTrue: [ 
			patchSet apply.
			^ self ].
	patchSet setupForNewClassVersionUpdates.
	patchSet classesWithNewVersions
		do: [ :each | 
			each
				updatePatchesForNewClassVersion: aProjectSetModification 
					patchSet: patchSet;
				updateNewClassVersionPatchesForExtensionsIn: aProjectSetModification
					patchSet: patchSet;
				updateNewClassVersionPatchesForSubclassesIn: aProjectSetModification
					patchSet: patchSet ].
	visitor := visitorClass new.
	visitor visit: aProjectSetModification.
	newClassVersionPatchSet := visitor patchSet.
	newClassVersionPatchSet applyForNewClassVersions: instanceMigrator
]

{ #category : '*rowan-gemstone-loader-extensions' }
RwGsImage class >> symbolDictionaryRegistryClass [

	^ RwGsSymbolDictionaryRegistry
]

{ #category : '*rowan-gemstone-loader-extensions' }
RwGsImage class >> loadedClassAndSymbolDicitonaryForClass: class ifPresent: presentBlock ifAbsent: absentBlock [


        "scan the symbol list for a RwLoadedClass instances for the given compiled method"

        self symbolList
                do: [ :symbolDict |
                        symbolDict rowanSymbolDictionaryRegistry
                                ifNotNil: [ :registry |
                                        (registry classRegistry at: class classHistory ifAbsent: [  ])
                                                ifNotNil: [ :loadedClass | ^ presentBlock value: symbolDict value: loadedClass ] ] ].
        ^ absentBlock value
]

{ #category : '*rowan-gemstone-loader-extensions' }
RwGsImage class >> loadedClassForClass: class ifAbsent: absentBlock [

	"Lookup the given class in the classRegistry"

	| history |
	history := class classHistory.
	self symbolList
		do: [ :symbolDict | 
			symbolDict rowanSymbolDictionaryRegistry
				ifNotNil: [ :registry | 
					(registry classRegistry at: history ifAbsent: [  ])
						ifNotNil: [ :loadedClass | ^ loadedClass ] ] ].
	^ absentBlock value
]

{ #category : '*rowan-gemstone-loader-extensions' }
RwGsImage class >> loadedClassNamed: className ifFound: foundBlock ifAbsent: absentBlock [

	"scan the symbol list for a RwLoadedClass instance of the given name"

	| class loadedClass |
	class := self objectNamed: className.
	class ifNil: [ ^ absentBlock value ].
	loadedClass := self
		loadedClassForClass: class
		ifAbsent: [ ^ absentBlock value ].
	^ foundBlock value: loadedClass
]

{ #category : '*rowan-gemstone-loader-extensions' }
RwGsImage class >> removeLoadedClassExtensionsForClass: class [

	"The class has or will be deleted from the system, remove the loadedClassExtensions that refer
		to the given class"

	| history |
	history := class classHistory.
	self symbolList
		do: [ :symbolDict | 
			symbolDict rowanSymbolDictionaryRegistry
				ifNotNil: [ :registry | registry classExtensionRegistry removeKey: history ifAbsent: [  ] ] ]
]

{ #category : '*rowan-gemstone-loader-extensions' }
RwGsImage class >> loadedClassExtensionsForClass: class [

	"lookup the loadedClassExtensions for the given class"

	| history found |
	history := class classHistory.
	found := IdentitySet new.
	self symbolList
		do: [ :symbolDict | 
			symbolDict rowanSymbolDictionaryRegistry
				ifNotNil: [ :registry | 
					(registry classExtensionRegistry at: history ifAbsent: [  ])
						ifNotNil: [ :loadedClassExtensionSet | found addAll: loadedClassExtensionSet ] ] ].
	^ found
]

