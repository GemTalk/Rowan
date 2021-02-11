Extension { #name : 'RwPackageMove' }

{ #category : '*rowan-gemstone-loader-extensions-onlyv2' }
RwPackageMove >> addMovedPackageToPatchSet: aPatchSet [

	aPatchSet addPackageMove: self
]

{ #category : '*rowan-gemstone-loader-extensions-onlyv2' }
RwPackageMove >> movePackage: classesWithNewVersions [
	"Move the loaded package from one project to another. 
		(https://github.com/dalehenrich/Rowan/issues/680)"

	projectBefore name = projectAfter name
		ifTrue: [ 
			self
				error:
					'internal error - unexpected move for package when source and destination projects ('
						, projectBefore name , ') are the same' ].
	packageBefore name = packageAfter name
		ifFalse: [ 
			self error: 'internal error - unexpected move for package when source package ('.
			packageBefore name , ' and destination package (' , packageAfter name
				, ') are NOT the same' ].
	(Rowan image loadedRegistryForPackageNamed: packageAfter name)
		movePackage: packageAfter name
		toProjectNamed: projectAfter name
]
