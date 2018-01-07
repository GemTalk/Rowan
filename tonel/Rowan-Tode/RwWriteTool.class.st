Class {
	#name : 'RwWriteTool',
	#superclass : 'RwTool',
	#category : 'Rowan-Tode'
}

{ #category : 'man page' }
RwWriteTool >> manPage [
  ^ TDManPage
    commandName: 'write'
    sourceMethod: self class name asString , '>>manPage'
    fromString:
      'NAME
  rowan write - Write packages to repository
SYNOPSIS
  rowan write <spec-url>
DESCRIPTION
EXAMPLES
  rowan write Rowan
'
]

{ #category : 'smalltalk api' }
RwWriteTool >> writePackageSet: aRwPackageSet specification: aRwSpecification [
  | packageManager repo repositoryUrl |
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
  aRwPackageSet packages values
    do: [ :rwPackage | 
      | packageStructure |
      packageStructure := RwCypressPackageStructure fromPackage: rwPackage.
      repo writePackageStructure: packageStructure ].
  ^ specification
]

{ #category : 'smalltalk api' }
RwWriteTool >> writePackageSet: aRwPackageSet specUrl: aString [
  ^ self
    writePackageSet: aRwPackageSet
    specification: (RwSpecification fromUrl: aString)
]

{ #category : 'smalltalk api' }
RwWriteTool >> writeSpecification: aRwSpecification [
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

{ #category : 'smalltalk api' }
RwWriteTool >> writeSpecUrl: aString [

	^ self writeSpecification: (RwSpecification fromUrl: aString)
]
