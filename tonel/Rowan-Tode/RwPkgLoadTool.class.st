Class {
	#name : 'RwPkgLoadTool',
	#superclass : 'RwPackageTool',
	#category : 'Rowan-Tode'
}

{ #category : 'smalltalk api' }
RwPkgLoadTool >> loadPackageSetDefinition: packageSetDefinitionToLoad [

	| loadedPackageSet diff loadedEntitySet |
	loadedEntitySet := packageSetDefinitionToLoad deriveLoadedThings.
	loadedPackageSet := loadedEntitySet asPackageDefinitionSet.
	diff := packageSetDefinitionToLoad compareAgainstBase: loadedPackageSet.
	diff isEmpty
		ifTrue: [ 
			| msg |
			msg := 'The packages are already up to date'.
			self inform: msg.
			^ msg ].
	Rowan image applyModification: diff.
	^ diff
]

{ #category : 'man page' }
RwPkgLoadTool >> manPage [
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
