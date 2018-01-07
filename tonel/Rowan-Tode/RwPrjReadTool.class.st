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
  prj read - Read packages from repository
SYNOPSIS
  prj read <spec-url>
DESCRIPTION

  Read the packages specified in the spec and return an RwPackageSet of packages.

EXAMPLES
  prj read Rowan
'
]

{ #category : 'smalltalk api' }
RwPrjReadTool >> readPackageSetForSpecification: aRwSpecification [
  | packageManager repo repositoryUrl readPackages |
  self specification: aRwSpecification.
  packageManager := CypressEnvironmentPackageManager new.
  repositoryUrl := specification repositoryUrl
    ifNil: [ 
      ('file:' , specification repoSpec repositoryRootPath , '/'
        , specification repoPath , '/') asRwUrl ]
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
RwPrjReadTool >> readPackageSetForSpecUrl: aString [
  ^ self readPackageSetForSpecification: (RwSpecification fromUrl: aString)
]

{ #category : 'validation' }
RwPrjReadTool >> validate: aRwSpecification [
  super validate: aRwSpecification.
  self validateForRead: aRwSpecification
]
