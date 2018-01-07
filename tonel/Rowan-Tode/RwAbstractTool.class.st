Class {
	#name : 'RwAbstractTool',
	#superclass : 'Object',
	#category : 'Rowan-Tode'
}

{ #category : 'private' }
RwAbstractTool >> fileUtilities [
  ^ Rowan fileUtilities
]

{ #category : 'smalltalk api' }
RwAbstractTool >> help [

	^self manPage asText
]

{ #category : 'man page' }
RwAbstractTool >> manPage [
  self subclassResponsibility: #'manPage'
]
