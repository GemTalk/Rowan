Class {
	#name : 'RwPrjReadTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tode'
}

{ #category : 'man page' }
RwPrjReadTool >> manPage [
	^ TDManPage
		commandName: 'read'
		sourceMethod: self class name asString , '>>manPage'
		fromString:
			'NAME
  prj read - Read project from repository (disk -> defs)
SYNOPSIS
  prj read <spec-url>
DESCRIPTION

  Read the packages specified in the spec and return an RwPackageSet of packages.

EXAMPLES
  prj read Rowan
'
]

{ #category : 'smalltalk api' }
RwPrjReadTool >> readProjectSetForSpecification: aRwSpecification [

	| packageManager repo repositoryUrl projectDefinition |
	self specification: aRwSpecification.
	packageManager := CypressEnvironmentPackageManager new.
	repositoryUrl := specification repositoryUrl
		ifNil: [ 
			('cypress:' , specification repoSpec repositoryRootPath , '/'
				, specification repoPath , '/') asRwUrl ]
		ifNotNil: [ :urlString | urlString asRwUrl ].
	repo := repositoryUrl asRwRepository.
	projectDefinition := RwProjectDefinition newForSpecification: aRwSpecification.
	aRwSpecification packageNamesInLoadOrder
		do: [ :packageName | 
			| structureName reader snapshot packageDefinition |
			structureName := packageName , repo packageExtension.
			reader := repo reader.
			reader
				packageStructure: (RwCypressPackageStructure named: structureName);
				packageDirectory:
						(reader fileUtils
								directoryFromPath: structureName
								relativeTo: repo directoryPath);
				readPackageStructure.
			packageDefinition := reader packageStructure rwSnapshot.
			projectDefinition addPackage: packageDefinition ].
	^ RwProjectSetDefinition new
		addProject: projectDefinition;
		yourself
]

{ #category : 'smalltalk api' }
RwPrjReadTool >> readProjectSetForSpecUrl: aString [

	^ self readProjectSetForSpecification: (RwSpecification fromUrl: aString)
]

{ #category : 'validation' }
RwPrjReadTool >> validate: aRwSpecification [
  super validate: aRwSpecification.
  self validateForRead: aRwSpecification
]
