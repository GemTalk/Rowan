Class {
	#name : 'RwPkgReadTool',
	#superclass : 'RwPackageTool',
	#category : 'Rowan-Tools'
}

{ #category : 'man page' }
RwPkgReadTool >> manPage [
  ^ TDManPage
    commandName: 'xxx'
    sourceMethod: self class name asString , '>>manPage'
    fromString:
      'NAME
  pkg xxx - xxx
SYNOPSIS
  pkg xxx
DESCRIPTION
EXAMPLES
  pkg xxx 
'
]

{ #category : 'smalltalk api' }
RwPkgReadTool >> readPackageSetForSpecification: aRwSpecification [

	| repo repositoryUrl readPackages |
	self specification: aRwSpecification.
	repositoryUrl := specification repositoryUrl
		ifNil: [ 
			('cypress:' , specification repoSpec repositoryRootPath , '/'
				, specification repoPath , '/') asRwUrl ]
		ifNotNil: [ :urlString | urlString asRwUrl ].
	repo := repositoryUrl asRwRepository.
	readPackages := {}.
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
			readPackages add: packageDefinition ].
	^ (RwEntitySet withAll: readPackages) asPackageDefinitionSet
]

{ #category : 'validation' }
RwPkgReadTool >> validate: aRwSpecification [
  super validate: aRwSpecification.
  self validateForRead: aRwSpecification
]
