deprecated
writeDeprecatedProjectDefinition: projectDefinition

	| repo repositoryUrl |
	self deprecated: 'Use RwComponentProjectDefinition instead'.
	repositoryUrl := projectDefinition repositoryUrl
		ifNil: [ 
			projectDefinition repositoryRootPath
				ifNil: [ self error: 'repositoryUrl or repository rootPath must be defined in url' ]
				ifNotNil: [ :rootPath | ('cypress:' , rootPath , '/' , projectDefinition repoPath , '/') asRwUrl ] ]
		ifNotNil: [ :urlString | urlString asRwUrl ].
	repo := repositoryUrl asRwRepository.
	projectDefinition packages values
		do: [ :rwPackageDefinition | 
			| packageStructure |
			packageStructure := RwCypressPackageStructure
				fromPackage: rwPackageDefinition.
			repo writePackageStructure: packageStructure ]