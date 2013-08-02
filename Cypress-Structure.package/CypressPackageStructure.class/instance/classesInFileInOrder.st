accessing
classesInFileInOrder

	^(GsGeneralDependencySorter
		on: self classes
		dependsOn: [:candidate | candidate superclassName]
		dependent: [:candidate | candidate className])
			inOrder