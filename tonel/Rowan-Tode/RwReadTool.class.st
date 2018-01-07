Class {
	#name : 'RwReadTool',
	#superclass : 'RwTool',
	#category : 'Rowan-Tode'
}

{ #category : 'man page' }
RwReadTool >> manPage [
  ^ TDManPage
    commandName: 'read'
    sourceMethod: self class name asString , '>>manPage'
    fromString:
      'NAME
  rowan read - Read packages from repository
SYNOPSIS
  rowan read <spec-url>
DESCRIPTION

  Read the packages specified in the spec and return an RwPackageSet of packages.

EXAMPLES
  rowan read Rowan
'
]

{ #category : 'smalltalk api' }
RwReadTool >> readPackageSetForSpecification: aRwSpecification [
  | packageManager repo repositoryUrl readPackages |
  self specification: aRwSpecification.
  packageManager := CypressEnvironmentPackageManager new.
  repositoryUrl := specification repositoryUrl
    ifNil: [ 
      specification repoSpec repositoryRootPath
        ifNil: [ self error: 'repositoryUrl or repository rootPath must be defined in url' ]
        ifNotNil: [ :rootPath | ('file:' , rootPath , '/' , specification repoPath , '/') asRwUrl ] ]
    ifNotNil: [ :urlString | urlString asRwUrl ].
  repo := repositoryUrl asRwRepository.
  packageManager
    defaultSymbolDictionaryName:
        (specification platformSpec at: 'gemstone') symbolDictName;
    defaultSymbolList: GsCurrentSession currentSession symbolList.
  readPackages := {}.
  aRwSpecification packageNames
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
  ^ (RwPackageSet withAll: readPackages) asDefinition
]

{ #category : 'smalltalk api' }
RwReadTool >> readPackageSetForSpecUrl: aString [
  ^ self readPackageSetForSpecification: (RwSpecification fromUrl: aString)
]
