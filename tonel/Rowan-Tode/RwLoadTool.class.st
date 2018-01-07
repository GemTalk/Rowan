Class {
	#name : 'RwLoadTool',
	#superclass : 'RwTool',
	#category : 'Rowan-Tode'
}

{ #category : 'image' }
RwLoadTool >> doUpdateLoadedPackageNames: aPackageNames [
  ^ specification imageSpec loadedPackages: aPackageNames
]

{ #category : 'smalltalk api' }
RwLoadTool >> loadSpecification: aRwSpecification [
  | packageSetToLoad loadedPackageSet diff |
  self specification: aRwSpecification.
  packageSetToLoad := RwTool read readPackageSetForSpecification: specification.
  loadedPackageSet := (Rowan loadedPackagesNamed: packageSetToLoad packageNames)
    asDefinition.
  diff := packageSetToLoad compareAgainstBase: loadedPackageSet.
  diff isEmpty
    ifTrue: [ 
      | msg |
      msg := 'The project ' , specification specName , ' is already up to date'.
      self inform: msg.
      ^ msg ].
  Rowan image applyModification: diff.
  specification
    updateLoadedCommitIdForTool: self;
    updateLoadedPackageNamesTool: self
      packageNames:
        (Rowan loadedPackagesNamed: packageSetToLoad packageNames) asDefinition
            packageNames.
  ^ diff
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

{ #category : 'smalltalk api' }
RwLoadTool >> validate: aRwSpecification [
  super validate: aRwSpecification.
  self validateForRead: aRwSpecification
]
