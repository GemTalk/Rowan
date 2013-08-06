"
notice: This work is protected by copyright. All rights reserved.
category: accessing
"
classesInFileInOrder

	^(GsGeneralDependencySorter
		on: self classes
		dependsOn: [:candidate | candidate superclassName]
		dependent: [:candidate | candidate className])
			inOrder