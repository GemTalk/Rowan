Class {
	#name : 'RwGsClassUnmanagedVersioningSymbolDictPatchV2',
	#superclass : 'RwGsClassVersioningSymbolDictPatchV2',
	#category : 'Rowan-GemStone-LoaderV2'
}

{ #category : 'patching' }
RwGsClassUnmanagedVersioningSymbolDictPatchV2 >> installNewClassVersionInSystem [
	"Install the new class association in the symbolAssociation for the class.
	 Update the LoadedClass with properties for the new classversion."

	self symbolDictionaryRegistry
		addNewUnmanagedClassVersionToAssociation: newClassVersion
		oldClassVersion: oldClassVersion
		toPackageNamed: self packageName
		implementationClass: RwGsSymbolDictionaryRegistry_ImplementationV2
]

{ #category : 'new version support' }
RwGsClassUnmanagedVersioningSymbolDictPatchV2 >> updatePatchesForNewClassVersion: aProjectSetModification patchSetSymbolList: patchSet [
	"
	we happen to be modifying the existing project modification, so I think
		it may be practical to fabricate a newVersionClassModification
		that matches the one that would have been created if the before
		class had been packaged.

	So a little ifNotNil error action and then convert the remainder of the 
		method to do what we need
	"

	| existingClass newVersionClassModification existingClassName movedDeletedMap |
	movedDeletedMap := Dictionary new.
	(patchSet class
		lookupSymbolDictName: self symbolDictionaryName
		in: patchSet movedClassesSymbolList)
		keysAndValuesDo: [ :className :classMove | movedDeletedMap at: className put: classMove ].
	existingClass := self oldClassVersion.
	existingClassName := existingClass name asString.

	(self existingSymbolDictionaryRegistry
		existingForClass: existingClass
		ifAbsent: [  ])
		ifNotNil: [ :loadedClass | 
			self
				error:
					'Expected the class ' , existingClassName printString
						, ' to be unmanaged, but found a loaded class in package '
						, loadedClass loadedPackage name printString ].

	newVersionClassModification := self classDefinition
		compareAgainstBaseForNewClassUnmanagedVersion: classModification before.
	newVersionClassModification isEmpty
		ifTrue: [ 
			self
				error:
					'Unexpectedly empty class modification for an unmanaged class '
						, existingClassName asString ]
		ifFalse: [ 
			"only newVersionClassModification with substance need further processing"
			aProjectSetModification
				classesModificationAndPackageModificationAndProjectModificationDo: [ :classesModification :packageModification | 
					classesModification isEmpty
						ifFalse: [ 
							| classesModified |
							classesModified := classesModification elementsModified.
							(classesModified at: existingClassName ifAbsent: [  ])
								ifNil: [ 
									"not unexpected ... if there are multiple packages involved"
									 ]
								ifNotNil: [ :existingClassModification | 
									| deleteClassModification |
									deleteClassModification := false.
									(movedDeletedMap at: existingClassName ifAbsent: [  ])
										ifNotNil: [ :classMove | 
											deleteClassModification := classMove packageBefore name
												= packageModification after name ].
									deleteClassModification
										ifTrue: [ classesModified removeKey: existingClassName ]
										ifFalse: [ 
											newVersionClassModification mergeForExistingClassWith: existingClassModification.
											classesModified
												at: existingClassName
												put: newVersionClassModification ] ] ] ] ]
]
