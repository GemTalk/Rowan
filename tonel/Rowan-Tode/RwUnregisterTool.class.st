Class {
	#name : 'RwUnregisterTool',
	#superclass : 'RwTool',
	#category : 'Rowan-Tode'
}

{ #category : 'man page' }
RwUnregisterTool >> manPage [
  ^ TDManPage
    commandName: 'unregister'
    sourceMethod: self class name asString , '>>manPage'
    fromString:
      'NAME
  rowan unregister - xxx
SYNOPSIS
  rowan unregister <spec-url>
DESCRIPTION
EXAMPLES
  rowan unregister \
    https://raw.githubusercontent.com/dalehenrich/CypressReferenceImplementation/rowan/rowanLoadSpec.ston
  rowan unregister \
    file:$GS_HOME/shared/repos/rowan/CypressReferenceImplementation/rowanLoadSpec.ston
  rowan unregister rowanLoadSpec
'
]

{ #category : 'smalltalk api' }
RwUnregisterTool >> unregisterSpecification: aRwSpecification [

  self specification: aRwSpecification.
  ^ specification unregister
]

{ #category : 'smalltalk api' }
RwUnregisterTool >> unregisterSpecUrl: aString [

	^ self unregisterSpecification: (RwSpecification fromUrl: aString)
]
