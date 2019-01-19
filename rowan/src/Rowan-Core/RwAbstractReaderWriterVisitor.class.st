Class {
	#name : 'RwAbstractReaderWriterVisitor',
	#superclass : 'Object',
	#instVars : [
		'currentProjectDefinition',
		'packageConvention',
		'currentPackageDefinition'
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

{ #category : 'validation' }
RwAbstractReaderWriterVisitor >> _validateRowanHybridProtocolConvention: protocol forPackageNamed: packageName [

	| canonProtocol expectedProtocol |
	(protocol at: 1) = $* ifFalse: [ ^ self ].
	canonProtocol := protocol asLowercase.
	expectedProtocol := '*', packageName asLowercase.
	canonProtocol = expectedProtocol ifTrue: [ ^ self ].
	self error: 'not yet implemented'
]

{ #category : 'validation' }
RwAbstractReaderWriterVisitor >> _validateRowanMonticelloProtocolConvention: protocol  forPackageNamed: packageName [

	| canonProtocol expectedProtocol |
	(protocol at: 1) = $* ifFalse: [ ^ self ].
	canonProtocol := protocol asLowercase.
	expectedProtocol := '*', packageName asLowercase.
	"off the top of my head"
	(canonProtocol beginsWith: expectedProtocol) 
		ifTrue: [
			(canonProtocol indexOf: $- startingAt: expectedProtocol size) <= 1 ifTrue: [ ^self ] ].
	self error: 'not yet implemented'
]

{ #category : 'accessing' }
RwAbstractReaderWriterVisitor >> currentPackageDefinition [

	^ currentPackageDefinition
]

{ #category : 'accessing' }
RwAbstractReaderWriterVisitor >> currentPackageDefinition: aRwPackageDefinition [

	currentPackageDefinition := aRwPackageDefinition
]

{ #category : 'accessing' }
RwAbstractReaderWriterVisitor >> currentProjectDefinition [

	^ currentProjectDefinition
]

{ #category : 'accessing' }
RwAbstractReaderWriterVisitor >> currentProjectDefinition: aRwComponentProjectDefinition [

	currentProjectDefinition := aRwComponentProjectDefinition
]

{ #category : 'accessing' }
RwAbstractReaderWriterVisitor >> packageConvention [

	"
		RowanHybrid	- [default] Class category is package name, method protocol with leading $* is case insensitive package name
		Monticello		- Class category is package name, method protocol with leading $* begins with case insensitive package name
		Rowan			- Class category and method protocol are not overloaded with packaging information
	"

	^ packageConvention ifNil: [ 
		| ppc dpc  |
		ppc := self currentProjectDefinition packageConvention.
		dpc := self _repositoryConventionFor: self packagesRoot.
		dpc ~= ppc
			ifTrue:  [ self error: 'Disk package convention ' , dpc printString, ' does not match expected package convention ', ppc printstring. ' for project ', self projectName printString ].
		packageConvention := ppc ]
]

{ #category : 'accessing' }
RwAbstractReaderWriterVisitor >> packagesRoot [

	^ self currentProjectDefinition packagesRoot
]

{ #category : 'accessing' }
RwAbstractReaderWriterVisitor >> projectName [

	^ self currentProjectDefinition projectName
]

{ #category : 'validation' }
RwAbstractReaderWriterVisitor >> validateMethodProtocol: protocol  forPackageNamed: packageName [

	"
		RowanHybrid	- [default] Class category is package name, method protocol with leading $* is case insensitive package name
		Monticello		- Class category is package name, method protocol with leading $* begins with case insensitive package name
		Rowan			- Class category and method protocol are not overloaded with packaging information
	"

	"signal an error if the protocol does not conform to the convention for the current project"

	self packageConvention = 'RowanHybrid'
		ifTrue: [ ^ self _validateRowanHybridProtocolConvention: protocol forPackageNamed: packageName ].
	self packageConvention = 'Monticello'
		ifTrue: [ ^ self _validateRowanMonticelloProtocolConvention: protocol forPackageNamed: packageName ].
	"Rowan - no convention ... any old protocol is fine"
]
