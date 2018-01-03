Class {
	#name : 'RwSaveTool',
	#superclass : 'RwTool',
	#category : 'Rowan-Tode'
}

{ #category : 'man page' }
RwSaveTool >> manPage [
  ^ TDManPage
    commandName: 'save'
    sourceMethod: self class name asString , '>>manPage'
    fromString:
      'NAME
  rowan save - Save image changes to repository
SYNOPSIS
  rowan save <spec-url>
DESCRIPTION
EXAMPLES
  rowan save \
    https://raw.githubusercontent.com/dalehenrich/CypressReferenceImplementation/rowan/rowanLoadSpec.ston
  rowan save \
    file:$GS_HOME/shared/repos/rowan/CypressReferenceImplementation/rowanLoadSpec.ston
  rowan save rowanLoadSpec
'
]

{ #category : 'smalltalk api' }
RwSaveTool >> specification: aRwSpecification [
  | packageManager repo |
  super specification: aRwSpecification.
  packageManager := CypressEnvironmentPackageManager new.
  repo := CypressAbstractRepository
    onUrl: (CypressUrl absoluteFromText: specification repositoryUrl)
    alias: ''.
  packageManager
    defaultSymbolDictionaryName: #'UserGlobals';
    defaultSymbolList: System myUserProfile symbolList.
  self packageNames
    do: [ :packageName | 
      | package |
      package := (CypressEnvironmentPackageDefinition named: packageName)
        lookupSymbolList: System myUserProfile symbolList;
        yourself.
      repo writePackageStructure: (CypressPackageStructure fromPackage: package) ]
]
