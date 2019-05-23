accessing
repositoryDefinition

	^ self properties at: 'repositoryDefinition' ifAbsentPut: [ self _repositoryDefinitionForProjectHome: self projectHome ]