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
RwPrjWriteTool >> writePackagesForSpecification: aRwSpecification [

	| packageSet |
	self specification: aRwSpecification.
	packageSet := self _symbolDictionaryForSpecification
		loadedPackageDefinitionsNamed: specification packageNamesInLoadOrder.
	self writePackageSet: packageSet specification: specification.
	^ specification
]

{ #category : 'smalltalk api' }
RwPrjWriteTool >> writePackagesForSpecUrl: aString [
  ^ self writePackagesForSpecification: (RwSpecification fromUrl: aString)
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

{ #category : 'cypress api' }
RwPrjWriteTool >> writeSpecification: aRwSpecification [

	"CYPRESS API: this messge unconditionally uses #Category to interpret package membership --- useful for saving Rowan code during development ONLY"

	| repo |
	self specification: aRwSpecification.
	repo := CypressAbstractRepository
		onUrl: (CypressUrl absoluteFromText: specification repositoryUrl)
		alias: ''.
	self packageNames
		do: [ :packageName | 
			| package |
			package := (CypressPackageDefinition named: packageName)
				lookupSymbolList: RwGsImage symbolList;
				yourself.
			repo writePackageStructure: (CypressPackageStructure fromPackage: package) ].
	^ specification
]

{ #category : 'cypress api' }
RwPrjWriteTool >> writeSpecUrl: aString [

	"CYPRESS API: this messge unconditionally uses #Category to interpret package membership --- useful for saving Rowan code during development ONLY"

	^ self writeSpecification: (RwSpecification fromUrl: aString)
]
