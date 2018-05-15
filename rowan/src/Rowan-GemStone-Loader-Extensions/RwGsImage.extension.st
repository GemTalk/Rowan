Extension { #name : 'RwGsImage' }

{ #category : '*rowan-gemstone-loader-extensions' }
RwGsImage class >> _bootstrapApplyModification: aProjectSetModification instanceMigrator: instanceMigrator [

	| visitor patchSet |
	visitor := RwGsImageRowanBootstrapPatchVisitor new.
	visitor visit: aProjectSetModification.
	patchSet := visitor patchSet.
	patchSet classesWithNewVersions isEmpty
		ifTrue: [ 
			patchSet apply.
			^ self ].
	self error: 'Did not expect new class versions during a _bootstrap load'

]

{ #category : '*rowan-gemstone-loader-extensions' }
RwGsImage class >> _cloneRowanLoaderSymbolDictionary [

	"in order to cleanly update Rowan using Rowan, it is necessary to isolate a copy of all of the loader code
		in a separate symbol dictionary and then use the isolated copy to execute the update."

	| rowanLoaderSymbolDict clonedSymDictName clonedSymDict |
Transcript cr; show: 'CLONING ROWANLOADER'.
	rowanLoaderSymbolDict := Rowan globalNamed: 'RowanLoader'.
	clonedSymDictName := #'RowanLoader_cloned'.
	clonedSymDict := rowanLoaderSymbolDict 
		_rowanCloneSymbolDictionaryNamed: clonedSymDictName 
		symbolList: GsCurrentSession currentSession symbolList.
Transcript show: ' --> ', clonedSymDict asOop printString.
	^ clonedSymDict

]

{ #category : '*rowan-gemstone-loader-extensions' }
RwGsImage class >> applyModification: aProjectSetModification instanceMigrator: instanceMigrator [

	true
		ifTrue: [ 
			self 
				applyModification: aProjectSetModification 
				visitorClass: RwGsImagePatchVisitor 
				instanceMigrator: instanceMigrator ]
		ifFalse: [ 
			| visitorClass |
			visitorClass := self _cloneRowanLoaderSymbolDictionary at: #RwGsImagePatchVisitor.
			self 
				applyModification: aProjectSetModification 
				visitorClass:visitorClass 
				instanceMigrator: instanceMigrator]

]

{ #category : '*rowan-gemstone-loader-extensions' }
RwGsImage class >> applyModification: aProjectSetModification visitorClass: visitorClass instanceMigrator: instanceMigrator [

	| visitor patchSet |
	visitor := visitorClass new.
	visitor visit: aProjectSetModification.
	patchSet := visitor patchSet.
	patchSet moveClassesBetweenPackages. "updates classesWithNewVersions, if a move is detected"
	patchSet classesWithNewVersions isEmpty
		ifTrue: [ 
			patchSet apply.
			^ self ].
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
	patchSet := visitor patchSet.
	patchSet applyForNewClassVersions: instanceMigrator

]
