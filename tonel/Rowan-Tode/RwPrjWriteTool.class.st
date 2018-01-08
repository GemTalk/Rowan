Class {
	#name : 'RwPrjWriteTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tode'
}

{ #category : 'man page' }
RwPrjWriteTool >> manPage [
  ^ TDManPage
    commandName: 'write'
    sourceMethod: self class name asString , '>>manPage'
    fromString:
      'NAME
  prj write - Write packages to repository
SYNOPSIS
  prj write <spec-url>
DESCRIPTION
EXAMPLES
  prj write Rowan
'
]

{ #category : 'smalltalk api' }
RwPrjWriteTool >> validate: aRwSpecification [
  super validate: aRwSpecification.
  self validateForWrite: aRwSpecification
]

{ #category : 'smalltalk api' }
RwPrjWriteTool >> writePackageSet: aRwPackageSet specification: aRwSpecification [
  | packageManager repo repositoryUrl |
  self specification: aRwSpecification.
  packageManager := CypressEnvironmentPackageManager new.
  repositoryUrl := specification repositoryUrl
    ifNil: [ 
      specification repoSpec repositoryRootPath
        ifNil: [ self error: 'repositoryUrl or repository rootPath must be defined in url' ]
        ifNotNil: [ :rootPath | ('cypress:' , rootPath , '/' , specification repoPath , '/') asRwUrl ] ]
    ifNotNil: [ :urlString | urlString asRwUrl ].
  repo := repositoryUrl asRwRepository.
  packageManager
    defaultSymbolDictionaryName:
        (specification platformSpec at: 'gemstone') symbolDictName;
    defaultSymbolList: GsCurrentSession currentSession symbolList.
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
  packageSet := (Rowan loadedPackagesNamed: specification packageNames)
    asDefinition.
  self writePackageSet: packageSet specification: specification.
  ^ specification
]

{ #category : 'smalltalk api' }
RwPrjWriteTool >> writePackagesForSpecUrl: aString [
  ^ self writePackagesForSpecification: (RwSpecification fromUrl: aString)
]

{ #category : 'cypress api' }
RwPrjWriteTool >> writeSpecification: aRwSpecification [ 
  | packageManager repo |
  self specification: aRwSpecification.
  packageManager := CypressEnvironmentPackageManager new.
  repo := CypressAbstractRepository
    onUrl: (CypressUrl absoluteFromText: specification repositoryUrl)
    alias: ''.
  packageManager
    defaultSymbolDictionaryName:
        (specification platformSpec at: 'gemstone') symbolDictName;
    defaultSymbolList: GsCurrentSession currentSession symbolList.
  self packageNames
    do: [ :packageName | 
      | package |
      package := (CypressEnvironmentPackageDefinition named: packageName)
        lookupSymbolList: GsCurrentSession currentSession symbolList;
        yourself.
      repo writePackageStructure: (CypressPackageStructure fromPackage: package) ].
^ specification
]

{ #category : 'cypress api' }
RwPrjWriteTool >> writeSpecUrl: aString [ 

^ self writeSpecification: (RwSpecification fromUrl: aString)
]
