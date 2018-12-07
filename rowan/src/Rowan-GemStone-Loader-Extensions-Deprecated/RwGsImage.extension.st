Extension { #name : 'RwGsImage' }

{ #category : '*rowan-gemstone-loader-extensions-deprecated' }
RwGsImage class >> applyModification: aProjectSetModification instanceMigrator: instanceMigrator [

	self deprecated: 'RwGsImage class >> applyModification:instanceMigrator: deprecated in Rowan 1.2.6+'.
	(self _shouldCloneRowanLoader: aProjectSetModification)
		ifTrue: [ 
			| visitorClass |
			visitorClass := self _cloneRowanLoaderSymbolDictionary at: #RwGsImagePatchVisitor.
			self 
				applyModification: aProjectSetModification 
				visitorClass:visitorClass 
				instanceMigrator: instanceMigrator ]
		ifFalse: [ 
			self 
				applyModification: aProjectSetModification 
				visitorClass: RwGsImagePatchVisitor 
				instanceMigrator: instanceMigrator ]
]

{ #category : '*rowan-gemstone-loader-extensions-deprecated' }
RwGsImage class >> applyModification: aProjectSetModification visitorClass: visitorClass instanceMigrator: instanceMigrator [

	| visitor patchSet newClassVersionPatchSet |
	self deprecated: 'RwGsImage class >> applyModification:visitorClass:instanceMigrator: deprecated in Rowan 1.2.6+'.
	visitor := visitorClass new.
	visitor visit: aProjectSetModification.
	patchSet := visitor patchSet.
	patchSet moveClassesBetweenPackages.
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
	newClassVersionPatchSet := visitor patchSet.
	newClassVersionPatchSet moveClassesWithNewVersionsBetweenPackages.
	newClassVersionPatchSet applyForNewClassVersions: instanceMigrator
]
