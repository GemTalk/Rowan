Class {
	#name : 'RwPrjRegisterTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tode'
}

{ #category : 'man page' }
RwPrjRegisterTool >> manPage [
  ^ TDManPage
    commandName: 'register'
    sourceMethod: self class name asString , '>>manPage'
    fromString:
      'NAME
  prj register - Register the specification
SYNOPSIS
  prj register [--name=<spec-name>] <spec-url>
DESCRIPTION
EXAMPLES
  prj register \
    https://raw.githubusercontent.com/dalehenrich/CypressReferenceImplementation/prj/rowanLoadSpec.ston
  prj register \
    file:$GS_HOME/shared/repos/rowan/CypressReferenceImplementation/rowanLoadSpec.ston

  prj register --name=Rowan \
    https://raw.githubusercontent.com/dalehenrich/CypressReferenceImplementation/rowan/rowanLoadSpec.ston
  prj register --name=Rowan \
    file:$GS_HOME/shared/repos/rowan/CypressReferenceImplementation/rowanLoadSpec.ston

  prj register --name=OldRowan Rowan
'
]

{ #category : 'smalltalk api' }
RwPrjRegisterTool >> registerSpecification: aRwSpecification [

  self specification: aRwSpecification.
  ^ specification register
]

{ #category : 'smalltalk api' }
RwPrjRegisterTool >> registerSpecification: aRwSpecification as: aSpecName [
  self registerSpecification: aRwSpecification copy.

]

{ #category : 'smalltalk api' }
RwPrjRegisterTool >> registerSpecUrl: aString as: aSpecName [
  ^ self registerSpecification: (RwSpecification fromUrl: aString) copy.

]
