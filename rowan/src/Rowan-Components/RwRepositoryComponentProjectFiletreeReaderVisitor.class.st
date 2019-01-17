Class {
	#name : 'RwRepositoryComponentProjectFiletreeReaderVisitor',
	#superclass : 'RwRepositoryComponentProjectReaderVisitor',
	#instVars : [
		'packageExtension'
	],
	#category : 'Rowan-Components'
}

{ #category : 'private' }
RwRepositoryComponentProjectFiletreeReaderVisitor >> _repositoryPropertyDictFor: packagesRoot [

	^ self _readObjectFrom: packagesRoot / '.filetree'
]

{ #category : 'accessing' }
RwRepositoryComponentProjectFiletreeReaderVisitor >> packageExtension [

	^ packageExtension 
		ifNil: [ 
			| ext |
			ext := (self _repositoryPropertyDictFor: currentProjectReferenceDefinition packagesRoot) at: 'packageExtension'.
			packageExtension := ext copyFrom: 2 to: ext size ]
]

{ #category : 'class reading' }
RwRepositoryComponentProjectFiletreeReaderVisitor >> readClassDirectory: classDirectory inPackage: packageName [

	| classProperties classComment normalizedMetadata classDefinition |
	classProperties :=  self _readObjectFrom: classDirectory / 'properties' , 'json'.
	classComment := (classDirectory / 'README' , 'md') contents.
	normalizedMetadata := Dictionary new.
	classProperties keysAndValuesDo: [:key :value |
		normalizedMetadata at: key asLowercase asSymbol put: value ].
	classDefinition :=	self newClassDefinitionFrom: {
		nil.
		classComment. 
		nil. 
		normalizedMetadata at: 'type' ifAbsent: ['normal']. 
		nil. 
		normalizedMetadata
	}.

	self error: 'not yet implemented'
]

{ #category : 'class reading' }
RwRepositoryComponentProjectFiletreeReaderVisitor >> readClassesFor: packageName packageRoot: packageRoot [

	currentProjectDefinition 
		packageNamed: packageName 
		ifAbsent: [ currentProjectDefinition addPackageNamed: packageName ].
	packageRoot directories do: [:directory |
		| directoryExtension |
		directoryExtension := directory extension.
		directoryExtension = 'class'
			ifTrue: [ self readClassDirectory: directory inPackage: packageName ]
			ifFalse: [
				directoryExtension = 'extension'
					ifTrue: [ self readClassExtensionDirectory: directory  inPackage: packageName ] ] ]
]

{ #category : 'class reading' }
RwRepositoryComponentProjectFiletreeReaderVisitor >> readClassExtensionDirectory: classExtensionDirectory inPackage: packageName [

	| classExtensionProperties className package classExtensionDef |
	classExtensionProperties :=  self _readObjectFrom: classExtensionDirectory / 'properties' , 'json'.
	className := classExtensionProperties at: 'name'.
	package := currentProjectDefinition packageNamed: packageName.
	classExtensionDef := package 
		classExtensionDefinitionNamed: className 
		ifAbsent: [ package addClassExtensionNamed: className ].

	self error: 'not yet implemented'
]
