Class {
	#name : 'RwRegisterTool',
	#superclass : 'RwTool',
	#category : 'Rowan-Tode'
}

{ #category : 'man page' }
RwRegisterTool >> manPage [
  ^ TDManPage
    commandName: 'register'
    sourceMethod: self class name asString , '>>manPage'
    fromString:
      'NAME
  rowan register - Register the specification
SYNOPSIS
  rowan register [--name=<spec-name>] <spec-url>
DESCRIPTION
EXAMPLES
  rowan register \
    https://raw.githubusercontent.com/dalehenrich/CypressReferenceImplementation/rowan/rowanLoadSpec.ston
  rowan register \
    file:$GS_HOME/shared/repos/rowan/CypressReferenceImplementation/rowanLoadSpec.ston

  rowan register --name=Rowan \
    https://raw.githubusercontent.com/dalehenrich/CypressReferenceImplementation/rowan/rowanLoadSpec.ston
  rowan register --name=Rowan \
    file:$GS_HOME/shared/repos/rowan/CypressReferenceImplementation/rowanLoadSpec.ston
'
]

{ #category : 'smalltalk api' }
RwRegisterTool >> specification: aRwSpecification [
  "Note that specUrl: takes a string argument that calls this method, if you don't <aRwSpecification> handy"

  super specification: aRwSpecification.
  specification register
]

{ #category : 'smalltalk api' }
RwRegisterTool >> specification: aRwSpecification as: aSpecName [
  super specification: aRwSpecification copy.
  specification specName: aSpecName.
  specification register
]

{ #category : 'smalltalk api' }
RwRegisterTool >> specUrl: aString as: aSpecName [
  super specification: (RwSpecification fromUrl: aString) copy.
  specification specName: aSpecName.
  specification register
]
