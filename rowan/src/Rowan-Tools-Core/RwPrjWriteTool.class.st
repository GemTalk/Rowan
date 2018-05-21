Class {
	#name : 'RwPrjWriteTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools-Core'
}

{ #category : 'private' }
RwPrjWriteTool >> _loadedPackagesForProject: projectDefinition [

	"Answer a PackageSet containing packages for the given project definition."

	^ self
		_loadedPackagesNamed: projectDefinition packageNames
		forProject: projectDefinition

]

{ #category : 'private' }
RwPrjWriteTool >> _loadedPackagesNamed: someNames forProject: projectDefinition [

	"Answer a PackageSet containing packages with the given names in the image.
 	If any name is not a loaded package, that name is ignored."

	"very likely we should be dealing with a project name at this point ... unless there is a use 
		case for loading a collection of packages without directly using the project interface 
		--- loaded packages do know what project they belong to ... so there might be something 
		done behind the scenes"

	| loadedPackages |
	loadedPackages := someNames
		collect: [ :aName | Rowan image loadedPackageNamed: aName ifAbsent: [ nil ] ].
	^ RwEntitySet
		withAll:
			(loadedPackages
				select: [ :loadedPackage | 
					loadedPackage notNil
						and: [ loadedPackage loadedProject name = projectDefinition name ] ])

]

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
	(self _loadedPackagesForProject: projectDefinition) asPackageDefinitionSet
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

]
