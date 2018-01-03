Class {
	#name : 'RwPackageLoadTool',
	#superclass : 'RwAbstractLoadTool',
	#category : 'Rowan-Tode'
}

{ #category : 'man page' }
RwPackageLoadTool >> manPage [
	^TDManPage
		commandName: 'load'
    	sourceMethod: self class name asString , '>>manPage'
    	fromString:
'NAME
  rowan package load - xxx
SYNOPSIS
  rowan package load [-h|--help] --spec=<spec-name> [<package-names>]
DESCRIPTION
EXAMPLES
  ./rowan package load --help
  ./rowan package load -h

  ./rowan package load \\
      --spec=https://raw.githubusercontent.com/dalehenrich/CypressReferenceImplementation/rowan/rowanLoadSpec.ston
  ./rowan package load rowanLoadSpec  Cypress-Core Rowan-Core 
'
]

{ #category : 'command api' }
RwPackageLoadTool >> spec: specName [
  ^ self spec: specName packageNames: nil
]

{ #category : 'command api' }
RwPackageLoadTool >> spec: specName packageNames: packageNamesOrNil [
  self halt
]
