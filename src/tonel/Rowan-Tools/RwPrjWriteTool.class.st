Class {
	#name : 'RwPrjWriteTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools'
}

{ #category : 'smalltalk api' }
RwPrjWriteTool >> validate: aRwSpecification [
  super validate: aRwSpecification.
  self validateForWrite: aRwSpecification
]

{ #category : 'smalltalk api' }
RwPrjWriteTool >> writePackageSet: aRwPackageSet specification: aRwSpecification [

	| repo repositoryUrl |
	self specification: aRwSpecification.
	repositoryUrl := specification repositoryUrl
		ifNil: [ 
			specification repoSpec repositoryRootPath
				ifNil: [ self error: 'repositoryUrl or repository rootPath must be defined in url' ]
				ifNotNil: [ :rootPath | ('cypress:' , rootPath , '/' , specification repoPath , '/') asRwUrl ] ]
		ifNotNil: [ :urlString | urlString asRwUrl ].
	repo := repositoryUrl asRwRepository.
	aRwPackageSet packages values
		do: [ :rwPackage | 
			| packageStructure |
			packageStructure := RwCypressPackageStructure fromPackage: rwPackage.
			repo writePackageStructure: packageStructure ].
	^ specification
]

{ #category : 'smalltalk api' }
RwPrjWriteTool >> writeProjectDefinition: projectDefinition [
	"project defintion does not necessarily represent a loaded project, so the loaded projects and packages 
		will not be marked not dirty"

	| repo repositoryUrl |
	repositoryUrl := projectDefinition repositoryUrl
		ifNil: [ 
			projectDefinition repositoryRootPath
				ifNil: [ self error: 'repositoryUrl or repository rootPath must be defined in url' ]
				ifNotNil: [ :rootPath | ('cypress:' , rootPath , '/' , projectDefinition repoPath , '/') asRwUrl ] ]
		ifNotNil: [ :urlString | urlString asRwUrl ].
	repo := repositoryUrl asRwRepository.
	(Rowan loadedPackagesForProject: projectDefinition) asPackageDefinitionSet
		do: [ :rwPackageDefinition | 
			| packageStructure |
			packageStructure := RwCypressPackageStructure
				fromPackage: rwPackageDefinition.
			repo writePackageStructure: packageStructure ]

]

{ #category : 'smalltalk api' }
RwPrjWriteTool >> writeProjectNamed: projectName [

	| loadedProject repo repositoryUrl projectDefinition packageDefinitionSet loadedPackages |
	loadedProject := Rowan image loadedProjectNamed: projectName.
	projectDefinition := loadedProject asDefinition.
	repositoryUrl := projectDefinition repositoryUrl
		ifNil: [ 
			projectDefinition repositoryRootPath
				ifNil: [ self error: 'repositoryUrl or repository rootPath must be defined in url' ]
				ifNotNil: [ :rootPath | ('cypress:' , rootPath , '/' , projectDefinition repoPath , '/') asRwUrl ] ]
		ifNotNil: [ :urlString | urlString asRwUrl ].
	repo := repositoryUrl asRwRepository.
	loadedPackages := Rowan loadedPackagesForProject: projectDefinition.
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

]
