Class {
	#name : 'RwPrjReadTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools'
}

{ #category : 'smalltalk api' }
RwPrjReadTool >> readProjectSetForProjectNamed: projectName [

	| projectDefinition repo |
	projectDefinition := (Rowan image loadedProjectNamed: projectName) asDefinition.
	repo := projectDefinition repositoryUrl asRwRepository.
	projectDefinition packageNamesInLoadOrder
		do: [ :packageName | 
			| structureName reader packageDefinition |
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
			projectDefinition addOrUpdatePackage: packageDefinition ].
	^ RwProjectSetDefinition new
		addProject: projectDefinition;
		yourself
]

{ #category : 'smalltalk api' }
RwPrjReadTool >> readProjectSetForSpecification: aRwSpecification [

	| repo repositoryUrl projectDefinition |
	self specification: aRwSpecification.
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
