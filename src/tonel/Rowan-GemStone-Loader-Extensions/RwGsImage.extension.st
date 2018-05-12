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
RwGsImage class >> applyModification: aProjectSetModification enableInstanceMigration: enableInstanceMigration [

	| visitor patchSet |
	visitor := RwGsImagePatchVisitor new.
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
	visitor := RwGsImagePatchVisitor new.
	visitor visit: aProjectSetModification.
	patchSet := visitor patchSet.
	patchSet applyForNewClassVersions: enableInstanceMigration

]

{ #category : '*rowan-gemstone-loader-extensions' }
RwGsImage class >> applyModification: aProjectSetModification instanceMigrator: instanceMigrator [

	| visitor patchSet |
	visitor := RwGsImagePatchVisitor new.
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
	visitor := RwGsImagePatchVisitor new.
	visitor visit: aProjectSetModification.
	patchSet := visitor patchSet.
	patchSet applyForNewClassVersions: instanceMigrator

]


