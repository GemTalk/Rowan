sorting - private
mapCandidatesIntoGraphs

	| dependsOnKey dependentKey |
	candidates do: 
			[:each |
			| individualDependency |
			dependsOnKey := dependsOnConverter value: each.
			dependentKey := dependentConverter value: each.
			candidateAliasMap at: dependentKey put: each.
			individualDependencyMap at: dependsOnKey ifAbsentPut: [Dictionary new].
			individualDependencyMap at: dependentKey ifAbsentPut: [Dictionary new].
			individualDependency := individualDependencyMap
						associationAt: dependsOnKey.
			(dependencyGraphs includesKey: dependsOnKey)
				ifFalse: [dependencyGraphs add: individualDependency].
			individualDependency value
				add: (individualDependencyMap associationAt: dependentKey)]