deprecated
writeDeprecatedProjectNamed: projectName

	| loadedProject repo repositoryUrl projectDefinition packageDefinitionSet loadedPackages |
	self deprecated: 'Use RwComponentProjectDefinition instead'.
	loadedProject := Rowan image loadedProjectNamed: projectName.
	projectDefinition := loadedProject asDefinition.
	repositoryUrl := projectDefinition repositoryUrl
		ifNil: [ 
			projectDefinition repositoryRootPath
				ifNil: [ self error: 'repositoryUrl or repository rootPath must be defined in url' ]
				ifNotNil: [ :rootPath | ('cypress:' , rootPath , '/' , projectDefinition repoPath , '/') asRwUrl ] ]
		ifNotNil: [ :urlString | urlString asRwUrl ].
	repo := repositoryUrl asRwRepository.
	loadedPackages := self _loadedPackagesForProject: projectDefinition.
	packageDefinitionSet := loadedPackages asPackageDefinitionSet.
	packageDefinitionSet
		do: [ :rwPackageDefinition | 
			| packageStructure |
			packageStructure := RwCypressPackageStructure
				fromPackage: rwPackageDefinition.
			repo writePackageStructure: packageStructure ].
	"loaded project and loaded packages written to disk - mark them not dirty"
	loadedProject markNotDirty.
	loadedPackages do: [:loadedPackage | loadedPackage markNotDirty ].