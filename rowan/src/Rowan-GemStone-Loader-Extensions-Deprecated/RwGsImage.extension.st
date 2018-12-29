Extension { #name : 'RwGsImage' }

{ #category : '*rowan-gemstone-loader-extensions-deprecated' }
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

{ #category : '*rowan-gemstone-loader-extensions-deprecated' }
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
