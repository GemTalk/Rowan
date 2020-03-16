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

	| projectModification packageModification |
	projectModification := aProjectSetModification elementsModified at: 'Rowan' ifAbsent: [ ^ false ].
	packageModification := projectModification packagesModification elementsModified at: 'Rowan-GemStone-Loader' ifAbsent: [ ^ false ].
	^ packageModification isEmpty not

]

{ #category : '*rowan-gemstone-loader-extensions-onlyv2' }
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

{ #category : '*rowan-gemstone-loader-extensions-onlyv2' }
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
