Class {
	#name : 'RwPrjLoadTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tode'
}

{ #category : 'image' }
RwPrjLoadTool >> doUpdateLoadedPackageNames: aPackageNames [
  ^ specification imageSpec loadedPackages: aPackageNames
]

{ #category : 'smalltalk api' }
RwPrjLoadTool >> loadSpecification: aRwSpecification [
  | packageSetToLoad loadedPackageSet diff |
  self specification: aRwSpecification.
  packageSetToLoad := Rowan projectTools read readPackageSetForSpecification: specification.
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
RwPrjLoadTool >> loadSpecUrl: aString [
  ^ self loadSpecification: (RwSpecification fromUrl: aString)
]

{ #category : 'man page' }
RwPrjLoadTool >> manPage [
	^ TDManPage
		commandName: 'load'
		sourceMethod: self class name asString , '>>manPage'
		fromString:
			'NAME
  prj load - Read project from repository and install (disk -> image)
SYNOPSIS
  prj load [--loads=<load-expression>] <spec-url>
DESCRIPTION
EXAMPLES

  # first download and clone the project, then load
  prj clone --gitRoot=$GS_HOME/shared/repos \
    https://raw.githubusercontent.com/dalehenrich/CypressReferenceImplementation/rowan/Rowan.ston
  prj load Rowan

  prj load Rowan										# load default packages
  prj load --loads=ALL Rowan						# load all packages
  prj load --loads=`#( ''Rowan-Tode'')` Rowan		# load Rowan-Tode package in addition 
'
]

{ #category : 'public' }
RwPrjLoadTool >> packageTools [

	^self platform packageTools
]

{ #category : 'smalltalk api' }
RwPrjLoadTool >> validate: aRwSpecification [
  super validate: aRwSpecification.
  self validateForRead: aRwSpecification
]
