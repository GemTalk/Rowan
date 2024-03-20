Class {
	#name : 'RwRepositoryResolvedProjectFiletreeReaderVisitorV2',
	#superclass : 'RwRepositoryComponentProjectReaderVisitor',
	#instVars : [
		'packageExtension'
	],
	#category : 'Rowan-DefinitionsV2'
}

{ #category : 'class directory reading' }
RwRepositoryResolvedProjectFiletreeReaderVisitorV2 class >> readClassDirectories: directoryArray [

	^ self readClassDirectories: directoryArray  projectName: '___READ_CLASS_Project___' packageName: '___READ_CLASS_Package___'
]

{ #category : 'class directory reading' }
RwRepositoryResolvedProjectFiletreeReaderVisitorV2 class >> readClassDirectories: directoryArray projectName: projectName packageName: packageName [
	| visitor resolvedProject packageDefinition |
	resolvedProject := RwResolvedProjectV2 new
		projectName: projectName;
		packageConvention: 'Rowan';
		gemstoneSetDefaultSymbolDictNameTo: 'Globals';
		yourself.

	resolvedProject
		addLoadComponentNamed: 'Core'
		comment: 'Temporary project to hold class definitions read from disk'.

	1 to: directoryArray size do: [ :index | 
		| directory |
		directory := directoryArray at: index.
		packageDefinition := resolvedProject
			addPackageNamed: packageName , '_' , index asString
			toComponentNamed: 'Core'.
		visitor := self new
			currentProjectDefinition: resolvedProject;
			currentPackageDefinition: packageDefinition;
			_packageConvention: 'Rowan';
			yourself.
		visitor readClassDirectory: directory inPackage: packageDefinition name ].
	^ resolvedProject
]

{ #category : 'class directory reading' }
RwRepositoryResolvedProjectFiletreeReaderVisitorV2 class >> readClassDirectory: directory [

	^ self readClassDirectories: {directory}
]

{ #category : 'class directory reading' }
RwRepositoryResolvedProjectFiletreeReaderVisitorV2 class >> readClassDirectory: directory projectName: projectName packageName: packageName [

	^ self readClassDirectories: {directory} projectName: projectName packageName: packageName
]

{ #category : 'other' }
RwRepositoryResolvedProjectFiletreeReaderVisitorV2 >> _filetreeRepositoryPropertyDictFor: aPackagesRoot [

	^ self _readObjectFrom: aPackagesRoot / '.filetree'
]

{ #category : 'accessing' }
RwRepositoryResolvedProjectFiletreeReaderVisitorV2 >> packageExtension [

	^ packageExtension 
		ifNil: [ 
			| ext |
			ext := (self _filetreeRepositoryPropertyDictFor: currentProjectDefinition packagesRoot) at: 'packageExtension'.
			packageExtension := ext copyFrom: 2 to: ext size ]
]

{ #category : 'class reading' }
RwRepositoryResolvedProjectFiletreeReaderVisitorV2 >> readClassDirectory: classDirectory inPackage: packageName [
	| classDirectoryReference classProperties classComment normalizedMetadata classDef propertiesRef |
	classDirectoryReference := classDirectory asFileReference.
	propertiesRef := classDirectoryReference / 'properties' , 'json'.
	classProperties := self _readObjectFrom: propertiesRef.
	classComment := (classDirectoryReference / 'README' , 'md') contents.
	normalizedMetadata := Dictionary new.
	classProperties
		keysAndValuesDo: [ :key :value | normalizedMetadata at: key asLowercase asSymbol put: value ].
	normalizedMetadata
		at: #'_gsFileOffset' put: 0;
		at: #'_gsFileName' put: propertiesRef fullName;
		yourself.
	classDef := self
		newClassDefinitionFrom:
			{nil.
			(classComment isEmpty
				ifTrue: [ nil ]
				ifFalse: [ classComment ]).
			nil.
			(normalizedMetadata at: #'type' ifAbsent: [ 'normal' ]).
			nil.
			normalizedMetadata}.
	[ self validateClassCategory: classDef forPackageNamed: packageName ]
		on: Error
		do: [ :ex | 
			ex
				addText:
					(RwRepositoryResolvedProjectTonelReaderVisitorV2
						lineNumberStringForOffset: 0
						fileName: propertiesRef fullName).
			ex pass ].
	(currentProjectDefinition packageNamed: packageName)
		addClassDefinition: currentClassDefinition.

	self
		readMethodDirectories: classDirectoryReference
		forClassDefinition: classDef
		inPackage: packageName
]

{ #category : 'class reading' }
RwRepositoryResolvedProjectFiletreeReaderVisitorV2 >> readClassesFor: packageName packageRoot: packageRoot [

	currentPackageDefinition := currentProjectDefinition 
		packageNamed: packageName 
		ifAbsent: [ currentProjectDefinition addRawPackageNamed: packageName ].
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
RwRepositoryResolvedProjectFiletreeReaderVisitorV2 >> readClassExtensionDirectory: classExtensionDirectory inPackage: packageName [

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
RwRepositoryResolvedProjectFiletreeReaderVisitorV2 >> readMethodDirectories: classDirectory forClassDefinition: classDefinition inPackage: packageName [

	(self readMethodDirectory: classDirectory / 'instance' forClassDefinition: classDefinition isClassMeta: false inPackage: packageName) do: [:methodDefinition |
		classDefinition addInstanceMethodDefinition: methodDefinition ].
	(self readMethodDirectory: classDirectory / 'class' forClassDefinition: classDefinition isClassMeta: true inPackage: packageName) do: [:methodDefinition |
		classDefinition addClassMethodDefinition: methodDefinition ]
]

{ #category : 'method reading' }
RwRepositoryResolvedProjectFiletreeReaderVisitorV2 >> readMethodDirectory: methodDirectory forClassDefinition: classDefinition isClassMeta: isClassMeta inPackage: packageName [

	| methodDefinitions methodDefinitionStream |
	methodDefinitions := {}.
	methodDirectory exists ifFalse: [ ^ methodDefinitions ].

	methodDefinitionStream := methodDefinitions writeStreamPortable.
	methodDirectory files do: [:file |
		file extension = 'st'
			ifTrue: [ 
				| protocol methodSource methodStream methodDef offset |
				methodStream := file contents  readStreamPortable.
				protocol := methodStream nextLine.
        offset := methodStream position .
				methodSource := methodStream upToEnd.
				methodDef := RwMethodDefinition newForSource: methodSource protocol: protocol.
        methodDef offset: offset inFile: file pathString .
				self 
					validateMethodDefinitionProtocol: methodDef 
						className: classDefinition name
						isMeta: isClassMeta 
						forPackageNamed:  packageName.
				methodDefinitionStream nextPut: methodDef ] ].

	^ methodDefinitions
]
