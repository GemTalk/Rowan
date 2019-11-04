Class {
	#name : 'RwAbstractProjectDefinitionV2',
	#superclass : 'RwDefinition',
	#instVars : [
		'packages'
	],
	#category : 'Rowan-Components'
}

{ #category : 'instance creation' }
RwAbstractProjectDefinitionV2 class >> newForDiskBasedProjectNamed: aString [

	| spec |
	spec := RwSimpleProjectSpecification new
		specName: aString;
		repoSpec: RwDiskRepositorySpecification new;
		yourself.
	^ self newForSpecification: spec

]

{ #category : 'instance creation' }
RwAbstractProjectDefinitionV2 class >> newForGitBasedProjectNamed: aString [

	| spec |
	spec := RwSimpleProjectSpecification new
		specName: aString;
		yourself.
	^ self newForSpecification: spec

]

{ #category : 'instance creation' }
RwAbstractProjectDefinitionV2 class >> newForSpecification: aRwSpecification [

	^ self
		withProperties:
			(Dictionary new
				add: 'name' -> aRwSpecification specName;
				add: 'spec' -> aRwSpecification;
				yourself)
		packageDefinitions: Dictionary new
]

{ #category : 'instance creation' }
RwAbstractProjectDefinitionV2 class >> withProperties: properties packageDefinitions: packageDefinitions [

	^ self basicNew
		properties: properties;
		packages: packageDefinitions;
		yourself
]

{ #category : 'accessing' }
RwAbstractProjectDefinitionV2 >> addOrUpdatePackage: aPackageDefinition [

	| key |
	key := aPackageDefinition key.
	packages at: key put: aPackageDefinition
]

{ #category : 'accessing' }
RwAbstractProjectDefinitionV2 >> addPackage: aPackageDefinition [

	| key |
	key := aPackageDefinition key.
	(packages includesKey: key) ifTrue: [self error: 'Duplicate package'].
	^ packages at: key put: aPackageDefinition
]

{ #category : 'accessing' }
RwAbstractProjectDefinitionV2 >> addPackageNamed: packageName [

	^ self addPackage: (RwPackageDefinition newNamed: packageName)
]

{ #category : 'accessing' }
RwAbstractProjectDefinitionV2 >> addPackagesNamed: packageNames [

	packageNames
		do: [ :packageName | self addPackage: (RwPackageDefinition newNamed: packageName) ]
]

{ #category : 'properties' }
RwAbstractProjectDefinitionV2 >> canCommit [

	^ self projectRef canCommit
]

{ #category : 'properties' }
RwAbstractProjectDefinitionV2 >> comment [

	^ self projectRef comment
]

{ #category : 'properties' }
RwAbstractProjectDefinitionV2 >> comment: aString [

	self projectRef comment: aString
]

{ #category : 'properties' }
RwAbstractProjectDefinitionV2 >> configsPath [

	^ self projectRef configsPath

]

{ #category : 'properties' }
RwAbstractProjectDefinitionV2 >> configsPath: aDirectoryPathString [

	self projectRef configsPath: aDirectoryPathString

]

{ #category : 'properties' }
RwAbstractProjectDefinitionV2 >> configurationExportUrl [

	"Return an RwFileUrl suitable for saving the receiver's configuration in the appropriate configs directory"

	| exportPath |
	self repositoryUrl
		ifNil: [ self error: 'RepositoryUrl has not been set. Receiver has not bee associated with an existing Rowan project on disk.' ].
	exportPath := self repositoryRootPath , '/' , self configsPath , '/'.
	^('file:' , exportPath) asRwUrl

]

{ #category : 'properties' }
RwAbstractProjectDefinitionV2 >> configurationTemplate [

	"Return a generic RwProjectLoadConfiguration based on the list of packages in the reciever"

	| config groupNames |
	config := RwProjectLoadConfiguration new
		name: 'Default';
		projectName: self projectRef specName;
		yourself.
	groupNames := self projectRef defaultGroupNames.
	groupNames
		do: [:groupName | config addDefinedGroupName: groupName ].
	config 
		conditionalPackagesAtConditions: { Rowan platform basePlatformAttribute } 
		andGroup: (groupNames at: 1) 
		addPackageNames: self packageNames.
	^ config

]

{ #category : 'properties' }
RwAbstractProjectDefinitionV2 >> defaultConfigurationNames [

	^self projectRef defaultConfigurationNames
]

{ #category : 'properties' }
RwAbstractProjectDefinitionV2 >> defaultGroupNames [

	^self projectRef defaultGroupNames
]

{ #category : 'accessing' }
RwAbstractProjectDefinitionV2 >> definitionWithKey: aKey ifAbsent: absentBlock [

	^packages at: aKey ifAbsent: absentBlock
]

{ #category : 'initialization' }
RwAbstractProjectDefinitionV2 >> initialize [

	super initialize.
	packages := Dictionary new
]

{ #category : 'testing' }
RwAbstractProjectDefinitionV2 >> isEmpty [
	"Answer true if this definition does not actually define anything."

	^super isEmpty & packages isEmpty
]

{ #category : 'accessing' }
RwAbstractProjectDefinitionV2 >> key [
	"Answer an object that can be used to uniquely identify myself in the context of my container."

	^self projectRef projectName
]

{ #category : 'accessing' }
RwAbstractProjectDefinitionV2 >> keys [

	^packages keys
]

{ #category : 'actions' }
RwAbstractProjectDefinitionV2 >> load [
	"load the receiver into the image"

	^ Rowan projectTools load loadProjectDefinition: self
]

{ #category : 'actions' }
RwAbstractProjectDefinitionV2 >> load: instanceMigrator [
	"load the receiver into the image"

	^ Rowan projectTools load loadProjectDefinition: self instanceMigrator: instanceMigrator
]

{ #category : 'properties' }
RwAbstractProjectDefinitionV2 >> loadedCommitId [

	^ self projectRef loadedCommitId
]

{ #category : 'properties' }
RwAbstractProjectDefinitionV2 >> loadedConfigurationNames [

	^ self projectRef loadedConfigurationNames
]

{ #category : 'properties' }
RwAbstractProjectDefinitionV2 >> loadedConfigurationNames: anArray [

	^ self projectRef imageSpec loadedConfigurationNames: anArray
]

{ #category : 'properties' }
RwAbstractProjectDefinitionV2 >> loadedGroupNames [

	^ self projectRef loadedGroupNames
]

{ #category : 'properties' }
RwAbstractProjectDefinitionV2 >> loadedGroupNames: anArray [

	^ self projectRef imageSpec loadedGroupNames: anArray
]

{ #category : 'accessing' }
RwAbstractProjectDefinitionV2 >> packageNamed: aString [

	^ self
		packageNamed: aString
		ifAbsent: [ self error: 'A package named ' , aString printString , ' was not found.' ]
]

{ #category : 'accessing' }
RwAbstractProjectDefinitionV2 >> packageNamed: aString ifAbsent: absentBlock [

	^ self packages at: aString ifAbsent: absentBlock
]

{ #category : 'accessing' }
RwAbstractProjectDefinitionV2 >> packageNames [
  ^ self packages keys asArray
]

{ #category : 'accessing' }
RwAbstractProjectDefinitionV2 >> packageNames: packageNames [

	packageNames
		do: [ :packageName | self addPackageNamed: packageName ]
]

{ #category : 'accessing' }
RwAbstractProjectDefinitionV2 >> packages [

	^packages
]

{ #category : 'accessing' }
RwAbstractProjectDefinitionV2 >> packages: aPackageDefinitionDictionary [

	packages := aPackageDefinitionDictionary
]

{ #category : 'copying' }
RwAbstractProjectDefinitionV2 >> postCopy [

	| oldPackages |
	super postCopy.
	oldPackages := packages.
	packages := Dictionary new.
	oldPackages keysAndValuesDo: [:key : value | packages at: key put: value copy ] .
]

{ #category : 'accessing' }
RwAbstractProjectDefinitionV2 >> projectRef [
	^ self subclassResponsibility
]

{ #category : 'actions' }
RwAbstractProjectDefinitionV2 >> read [
	"refresh the contents of the receiver ... the reciever will match the definitions on disk based on the default component and group names"

	"return a project definition set that will contain the project definition"

	| projecSetDefinition |
	projecSetDefinition := RwProjectSetDefinition new.
	projecSetDefinition addProject: (Rowan projectTools read readProjectDefinition: self).
	^ projecSetDefinition
]

{ #category : 'tool api' }
RwAbstractProjectDefinitionV2 >> readProjectSet [

	^ Rowan projectTools read  readProjectSetForProjectDefinition: self
]

{ #category : 'reading' }
RwAbstractProjectDefinitionV2 >> readProjectSetReadTool: readTool withConfigurations: theConfigNames groupNames: theGroupNames [

	^ readTool
		readProjectSetForProjectDefinition: self
			withConfigurations: theConfigNames 
			groupNames: theGroupNames
]

{ #category : 'accessing' }
RwAbstractProjectDefinitionV2 >> removePackage: aPackageDefinition [

	| key |
	key := aPackageDefinition key.
	(packages includesKey: key) ifFalse: [self error: 'Package not present.'].
	packages removeKey: key

]

{ #category : 'accessing' }
RwAbstractProjectDefinitionV2 >> removePackageNamed: packageName [

	^self removePackage: (self packageNamed: packageName)

]

{ #category : 'properties' }
RwAbstractProjectDefinitionV2 >> repoPath [

	^ self projectRef repoPath
]

{ #category : 'properties' }
RwAbstractProjectDefinitionV2 >> repoPath: aDirectoryPathString [

	self projectRef repoPath: aDirectoryPathString

]

{ #category : 'properties' }
RwAbstractProjectDefinitionV2 >> repositoryRoot [
	"Root directory of the project. The configsPath, repoPath, specsPath, and projectsPath are specified relative to the repository root."

	^ self repositoryRootPath asFileReference
]

{ #category : 'properties' }
RwAbstractProjectDefinitionV2 >> repositoryRootPath [

	^ self projectRef repositoryRootPath
]

{ #category : 'properties' }
RwAbstractProjectDefinitionV2 >> repositoryRootPath: rootRepoPath [

	self projectRef repositoryRootPath: rootRepoPath
]

{ #category : 'properties' }
RwAbstractProjectDefinitionV2 >> repositoryUrl [

	^ self projectRef repositoryUrl
]

{ #category : 'properties' }
RwAbstractProjectDefinitionV2 >> repositoryUrl: aString [

	self projectRef repositoryUrl: aString
]

{ #category : 'properties' }
RwAbstractProjectDefinitionV2 >> specsPath [

	^ self projectRef specsPath
]

{ #category : 'properties' }
RwAbstractProjectDefinitionV2 >> specsPath: aDirectoryPathString [

	self projectRef specsPath: aDirectoryPathString

]

{ #category : 'actions' }
RwAbstractProjectDefinitionV2 >> unload [
	"unload the receiver from the image"

	^ Rowan projectTools delete deleteProjectDefinition: self copy	"delete operation is destructive to the projectDefinition"
]

{ #category : 'loading' }
RwAbstractProjectDefinitionV2 >> updateLoadedCommitId [

	| loadedCommitId |
	loadedCommitId := [  Rowan gitTools gitcommitShaIn: self repositoryRoot pathString ]
		on: Error
		do: [ :ignored | 
			"most likely no commits yet"
			^ nil ].
	self projectRef imageSpec loadedCommitId: loadedCommitId
]

{ #category : 'loading' }
RwAbstractProjectDefinitionV2 >> updateLoadedComponentInfoFor: aLoadedProject from: map [

	aLoadedProject
		loadedConfigurationNames: (map at: 'loadedConfigurationNames');
		loadedGroupNames: (map at: 'loadedGroupNames')
]

{ #category : 'properties' }
RwAbstractProjectDefinitionV2 >> useGit [

	^ self projectRef useGit
]
