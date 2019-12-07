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

	^ self readClassDirectories: directoryArray  projectName: '___READ_CLASS_Project___' packageName: '___READ_CLASS_Package___'
]

{ #category : 'class directory reading' }
RwRepositoryComponentProjectFiletreeReaderVisitor class >> readClassDirectories: directoryArray  projectName: projectName packageName: packageName [

	| visitor projectReference projectDefinition packageDefinition  |
	projectReference := RwProjectReferenceDefinition projectName: projectName useGit: false.
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

{ #category : 'class directory reading' }
RwRepositoryComponentProjectFiletreeReaderVisitor class >> readClassDirectory: directory projectName: projectName packageName: packageName [

	^ self readClassDirectories: {directory} projectName: projectName packageName: packageName
]

{ #category : 'other' }
RwRepositoryComponentProjectFiletreeReaderVisitor >> _filetreeRepositoryPropertyDictFor: aPackagesRoot [

	^ self _readObjectFrom: aPackagesRoot / '.filetree'
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
	self validateClassCategory: classDef forPackageNamed: packageName.
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

	(self readMethodDirectory: classDirectory / 'instance' forClassDefinition: classDefinition isClassMeta: false inPackage: packageName) do: [:methodDefinition |
		classDefinition addInstanceMethodDefinition: methodDefinition ].
	(self readMethodDirectory: classDirectory / 'class' forClassDefinition: classDefinition isClassMeta: true inPackage: packageName) do: [:methodDefinition |
		classDefinition addClassMethodDefinition: methodDefinition ]
]

{ #category : 'method reading' }
RwRepositoryComponentProjectFiletreeReaderVisitor >> readMethodDirectory: methodDirectory forClassDefinition: classDefinition isClassMeta: isClassMeta inPackage: packageName [

	| methodDefinitions methodDefinitionStream |
	methodDefinitions := {}.
	methodDirectory exists ifFalse: [ ^ methodDefinitions ].

	methodDefinitionStream := methodDefinitions writeStreamPortable.
	methodDirectory files do: [:file |
		file extension = 'st'
			ifTrue: [ 
				| protocol methodSource methodStream methodDef |
				methodStream := file contents  readStreamPortable.
				protocol := methodStream nextLine.
				methodSource := methodStream upToEnd.
				methodDef := RwMethodDefinition newForSource: methodSource protocol: protocol.
				self 
					validateMethodDefinitionProtocol: methodDef 
						className: classDefinition name
						isMeta: isClassMeta 
						forPackageNamed:  packageName.
				methodDefinitionStream nextPut: methodDef ] ].

	^ methodDefinitions
]
