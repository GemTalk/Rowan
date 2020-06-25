Class {
	#name : 'RwGsClassUnmanagedVersioningSymbolDictPatchV2',
	#superclass : 'RwGsClassVersioningSymbolDictPatchV2',
	#category : 'Rowan-GemStone-LoaderV2'
}

{ #category : 'new version support' }
RwGsClassUnmanagedVersioningSymbolDictPatchV2 >> updatePatchesForNewClassVersion: aProjectSetModification patchSetSymbolList: patchSet [
	| existingClass loadedClass loadedPackageName loadedClassDefinition newVersionClassModification existingClassName movedDeletedMap |
	movedDeletedMap := Dictionary new.
	(patchSet class
		lookupSymbolDictName: self symbolDictionaryName
		in: patchSet movedClassesSymbolList)
		keysAndValuesDo: [ :className :classMove | movedDeletedMap at: className put: classMove ].
	existingClass := self oldClassVersion.
	existingClassName := existingClass name asString.

	loadedClass := self existingSymbolDictionaryRegistry
		existingForClass: existingClass
		ifAbsent: [ 
			"
				we happen to be modifying the existing project modification, so I think
					it may be practical to fabricate a newVersionClassModification
					that matches the one that would have been created if the before
					class had been packaged.

				So a little ifNotNil error action and then convert the remainder of the 
					method to do what we need
			"
			self halt ].
	loadedClassDefinition := loadedClass asDefinition.
	loadedPackageName := loadedClass loadedPackage name.

	newVersionClassModification := self classDefinition
		compareAgainstBaseForNewClassVersion: loadedClassDefinition.
	newVersionClassModification isEmpty
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
