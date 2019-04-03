Class {
	#name : 'RwRepositoryComponentProjectFiletreeReaderVisitor',
	#superclass : 'RwRepositoryComponentProjectReaderVisitor',
	#instVars : [
		'packageExtension'
	],
	#category : 'Rowan-Components'
}

{ #category : 'class directory reading' }
RwRepositoryComponentProjectFiletreeReaderVisitor class >> readClassDirectories: directoryArray [

	| visitor projectReference projectDefinition packageDefinition packageName |
	packageName :=  '___READ_CLASS_Package___'.
	projectReference := RwProjectReferenceDefinition projectName: '___READ_CLASS_Project___' useGit: false.
	projectDefinition := (RwComponentProjectDefinition newForProjectReference: projectReference)
		packageConvention: 'Rowan';
		setSymbolDictName: 'Globals' forPackageNamed: packageName;
		yourself.
	1 to: directoryArray size do: [:index |
		| directory |
		directory := directoryArray at: index.
		projectDefinition setSymbolDictName: 'Globals' forPackageNamed: packageName, '_', index asString.
		packageDefinition := projectDefinition addPackageNamed:  packageName, '_', index asString.
		visitor := self new
			currentProjectDefinition: projectDefinition;
			currentPackageDefinition: packageDefinition;
			_packageConvention: 'Rowan';
			yourself.
		visitor readClassDirectory: directory inPackage: packageDefinition name ].
	^ projectDefinition
]

{ #category : 'class directory reading' }
RwRepositoryComponentProjectFiletreeReaderVisitor class >> readClassDirectory: directory [

	^ self readClassDirectories: {directory}
]

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

	| classDirectoryReference classProperties classComment normalizedMetadata classDef |
	classDirectoryReference := classDirectory asFileReference.
	classProperties :=  self _readObjectFrom: classDirectoryReference / 'properties' , 'json'.
	classComment := (classDirectoryReference / 'README' , 'md') contents.
	normalizedMetadata := Dictionary new.
	classProperties keysAndValuesDo: [:key :value |
		normalizedMetadata at: key asLowercase asSymbol put: value ].
	classDef :=	self newClassDefinitionFrom: {
		nil.
		classComment. 
		nil. 
		normalizedMetadata at: 'type' ifAbsent: ['normal']. 
		nil. 
		normalizedMetadata
	}.
	self validateClassCategory: classDef category  forPackageNamed: packageName.
	(currentProjectDefinition packageNamed: packageName)
		addClassDefinition: currentClassDefinition.

	self readMethodDirectories: classDirectoryReference forClassDefinition: classDef inPackage: packageName
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

	| classExtensionProperties className package |
	classExtensionProperties :=  self _readObjectFrom: classExtensionDirectory / 'properties' , 'json'.
	className := classExtensionProperties at: 'name'.
	package := currentProjectDefinition packageNamed: packageName.
	currentClassDefinition := nil.
	currentClassExtension := package 
		classExtensionDefinitionNamed: className 
		ifAbsent: [ package addClassExtensionNamed: className ].

	self readMethodDirectories: classExtensionDirectory forClassDefinition: currentClassExtension inPackage: packageName
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
