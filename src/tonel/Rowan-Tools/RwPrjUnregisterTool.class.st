Class {
	#name : 'RwPrjUnregisterTool',
	#superclass : 'RwProjectTool',
	#category : 'Rowan-Tools'
}

{ #category : 'man page' }
RwPrjUnregisterTool >> manPage [
  ^ TDManPage
    commandName: 'unregister'
    sourceMethod: self class name asString , '>>manPage'
    fromString:
      'NAME
  prj unregister - xxx
SYNOPSIS
  prj unregister <spec-url>
DESCRIPTION
EXAMPLES
  prj unregister Rowan
'
]

{ #category : 'smalltalk api' }
RwPrjUnregisterTool >> unregisterSpecification: aRwSpecification [

  self specification: aRwSpecification.
  ^ specification unregister
]

{ #category : 'smalltalk api' }
RwPrjUnregisterTool >> unregisterSpecUrl: aString [

	^ self unregisterSpecification: (RwSpecification fromUrl: aString)
]
