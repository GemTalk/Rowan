"
notice: This work is protected by copyright. All rights reserved.
category: sorting - private
"
determineGraphRoots

	^dependencyGraphs
		selectAssociations: [:each | (candidateAliasMap includesKey: each key) not]