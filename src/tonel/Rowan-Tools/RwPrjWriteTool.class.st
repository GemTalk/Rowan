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
RwPrjWriteTool >> writePackageSet: aRwPackageSet specUrl: aString [
  ^ self
    writePackageSet: aRwPackageSet
    specification: (RwSpecification fromUrl: aString)
]

{ #category : 'smalltalk api' }
RwPrjWriteTool >> writeProjectDefinition: projectDefinition [

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

	| loadedProject repo repositoryUrl projectDefinition |
	loadedProject := Rowan image loadedProjectNamed: projectName.
	projectDefinition := loadedProject asDefinition.
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
