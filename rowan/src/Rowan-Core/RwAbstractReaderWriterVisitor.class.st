Class {
	#name : 'RwAbstractReaderWriterVisitor',
	#superclass : 'Object',
	#instVars : [
		'currentProjectDefinition'
	],
	#category : 'Rowan-Core'
}

{ #category : 'accessing' }
 RwAbstractReaderWriterVisitor>> currentProjectDefinition [

	^ currentProjectDefinition
]

{ #category : 'accessing' }
 RwAbstractReaderWriterVisitor>> currentProjectDefinition: aRwComponentProjectDefinition [

	currentProjectDefinition := aRwComponentProjectDefinition
]
