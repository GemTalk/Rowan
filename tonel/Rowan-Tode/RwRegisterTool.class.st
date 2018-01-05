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

  rowan register --name=OldRowan Rowan
'
]

{ #category : 'smalltalk api' }
RwRegisterTool >> registerSpecification: aRwSpecification [

  self specification: aRwSpecification.
  ^ specification register
]

{ #category : 'smalltalk api' }
RwRegisterTool >> registerSpecification: aRwSpecification as: aSpecName [
  self registerSpecification: aRwSpecification copy.

]

{ #category : 'smalltalk api' }
RwRegisterTool >> registerSpecUrl: aString as: aSpecName [
  ^ self registerSpecification: (RwSpecification fromUrl: aString) copy.

]
