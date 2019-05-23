accessing
repositoryDefinition: aRepositoryDefinition

	aRepositoryDefinition ifNil: [ ^ self properties removeKey:  'repositoryDefinition' ifAbsent: [] ].
	^ self properties at: 'repositoryDefinition' put: aRepositoryDefinition