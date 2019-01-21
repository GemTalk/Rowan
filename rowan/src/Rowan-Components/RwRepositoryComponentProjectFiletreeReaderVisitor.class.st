Class {
	#name : 'RwRepositoryComponentProjectFiletreeReaderVisitor',
	#superclass : 'RwRepositoryComponentProjectReaderVisitor',
	#instVars : [
		'packageExtension'
	],
	#category : 'Rowan-Components'
}

{ #category : 'private' }
RwRepositoryComponentProjectFiletreeReaderVisitor >> _filetreeRepositoryPropertyDictFor: packagesRoot [

	^ self _readObjectFrom: packagesRoot / '.filetree'
]

{ #category : 'accessing' }
RwRepositoryComponentProjectFiletreeReaderVisitor >> packageExtension [

	^ packageExtension 
		ifNil: [ 
			| ext |
			ext := (self _filetreeRepositoryPropertyDictFor: currentProjectReferenceDefinition packagesRoot) at: 'packageExtension'.
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
	(currentProjectDefinition packageNamed: packageName)
		addClassDefinition: classDefinition.

	self readMethodDirectories: classDirectory forClassDefinition: classDefinition inPackage: packageName
]

{ #category : 'class reading' }
RwRepositoryComponentProjectFiletreeReaderVisitor >> readClassesFor: packageName packageRoot: packageRoot [

	currentPackageDefinition := currentProjectDefinition 
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

	self readMethodDirectories: classExtensionDirectory forClassDefinition: classExtensionDef inPackage: packageName
]

{ #category : 'method reading' }
RwRepositoryComponentProjectFiletreeReaderVisitor >> readMethodDirectories: classDirectory forClassDefinition: classDefinition inPackage: packageName [

	(self readMethodDirectory: classDirectory / 'instance' inPackage: packageName) do: [:methodDefinition |
		classDefinition addInstanceMethodDefinition: methodDefinition ].
	(self readMethodDirectory: classDirectory / 'class' inPackage: packageName) do: [:methodDefinition |
		classDefinition addClassMethodDefinition: methodDefinition ]
]

{ #category : 'method reading' }
RwRepositoryComponentProjectFiletreeReaderVisitor >> readMethodDirectory: methodDirectory inPackage: packageName [

	| methodDefinitions methodDefinitionStream |
	methodDefinitions := {}.
	methodDirectory exists ifFalse: [ ^ methodDefinitions ].

	methodDefinitionStream := methodDefinitions writeStreamPortable.
	methodDirectory files do: [:file |
		file extension = 'st'
			ifTrue: [ 
				| protocol methodSource methodStream |
				methodStream := file contents  readStreamPortable.
				protocol := methodStream nextLine.
				methodSource := methodStream upToEnd.
				self validateMethodProtocol: protocol  forPackageNamed: packageName.
				methodDefinitionStream nextPut: (RwMethodDefinition newForSource: methodSource protocol: protocol) ] ].

	^ methodDefinitions
]
