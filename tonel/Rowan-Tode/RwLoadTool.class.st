Class {
	#name : 'RwLoadTool',
	#superclass : 'RwTool',
	#category : 'Rowan-Tode'
}

{ #category : 'smalltalk api' }
RwLoadTool >> loadSpecification: aRwSpecification [
  | packageManager repo |
  self specification: aRwSpecification.
  packageManager := CypressEnvironmentPackageManager new.
  repo := CypressAbstractRepository
    onUrl: (CypressUrl absoluteFromText: specification repositoryUrl)
    alias: ''.
  packageManager
    defaultSymbolDictionaryName: #'UserGlobals';
    defaultSymbolList: System myUserProfile symbolList.
  self packageNames
    do: [ :packageName | 
      packageManager
        addResolvedReference:
          (CypressResolvedReference name: packageName repository: repo) ].
  packageManager loadResolvedReferences.
  ^ specification updateLoadedCommitIdForTool: self
]

{ #category : 'smalltalk api' }
RwLoadTool >> loadSpecUrl: aString [
  ^ self loadSpecification: (RwSpecification fromUrl: aString)
]

{ #category : 'man page' }
RwLoadTool >> manPage [
  ^ TDManPage
    commandName: 'load'
    sourceMethod: self class name asString , '>>manPage'
    fromString:
      'NAME
  rowan load - Load from the repository
SYNOPSIS
  rowan load [--loads=<load-expression>] <spec-url>
DESCRIPTION
EXAMPLES

  # first download and clone the project, then load
  rowan clone --gitRoot=$GS_HOME/shared/repos \
    https://raw.githubusercontent.com/dalehenrich/CypressReferenceImplementation/rowan/Rowan.ston
  rowan load Rowan

  rowan load Rowan										# load default packages
  rowan load --loads=ALL Rowan						# load all packages
  rowan load --loads=`#( ''Rowan-Tode'')` Rowan		# load Rowan-Tode package in addition 
'
]
