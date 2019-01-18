Class {
	#name : 'RwAbstractReaderWriterVisitor',
	#superclass : 'Object',
	#instVars : [
		'currentProjectDefinition'
	],
	#category : 'Rowan-Core'
}

{ #category : 'private' }
RwAbstractReaderWriterVisitor >> _repositoryConventionFor: packagesRoot [

	^ (self _repositoryPropertyDictFor: packagesRoot) at: #convention ifAbsent: [ 'RowanHybrid'  ]
]

{ #category : 'private' }
RwAbstractReaderWriterVisitor >> _repositoryFormatFor: packagesRoot [

	^ (self _repositoryPropertyDictFor: packagesRoot) at: #format ifAbsent: [ 'filetree' ]
]

{ #category : 'private' }
RwAbstractReaderWriterVisitor >> _repositoryPropertyDictFor: packagesRoot [

	| propertiesFile |
	propertiesFile := packagesRoot / 'properties', 'st'.
	propertiesFile exists
		ifFalse: [ ^ Dictionary new ].
	^ self _readObjectFrom: propertiesFile
]

{ #category : 'accessing' }
RwAbstractReaderWriterVisitor >> currentProjectDefinition [

	^ currentProjectDefinition
]

{ #category : 'accessing' }
RwAbstractReaderWriterVisitor >> currentProjectDefinition: aRwComponentProjectDefinition [

	currentProjectDefinition := aRwComponentProjectDefinition
]
