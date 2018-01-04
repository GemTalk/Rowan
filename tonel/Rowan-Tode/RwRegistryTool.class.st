Class {
	#name : 'RwRegistryTool',
	#superclass : 'RwTool',
	#category : 'Rowan-Tode'
}

{ #category : 'man page' }
RwRegistryTool >> manPage [
  ^ TDManPage
    commandName: 'registry'
    sourceMethod: self class name asString , '>>manPage'
    fromString:
      'NAME
  rowan registry - Inspect the spec registry
SYNOPSIS
  rowan registry [--names|--list]
DESCRIPTION
EXAMPLES
  rowan registry          # return registry dictionary
  rowan registry --list   # return an array of registered specs
  rowan registry --names  # return an array of registered spec names

  rowan registry;         edit
  rowan registry --list;  edit
  rowan registry --names; edit
'
]

{ #category : 'smalltalk api' }
RwRegistryTool >> registeredSpecNames [
  ^ self registry keys
]

{ #category : 'smalltalk api' }
RwRegistryTool >> registeredSpecs [
  ^ self registry values
]

{ #category : 'smalltalk api' }
RwRegistryTool >> registry [
  ^ RwSpecification registry
]
