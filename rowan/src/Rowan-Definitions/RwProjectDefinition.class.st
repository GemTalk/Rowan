Class {
	#name : 'RwProjectDefinition',
	#superclass : 'RwDefinition',
	#instVars : [
		'packages'
	],
	#category : 'Rowan-Definitions'
}

{ #category : 'instance creation' }
RwProjectDefinition class >> newForDiskBasedProjectNamed: aString [

	| spec |
	spec := RwSimpleProjectSpecification new
		specName: aString;
		repoSpec: RwDiskRepositorySpecification new;
		yourself.
	^ self newForSpecification: spec

]

{ #category : 'instance creation' }
RwProjectDefinition class >> newForGitBasedProjectNamed: aString [

	| spec |
	spec := RwSimpleProjectSpecification new
		specName: aString;
		yourself.
	^ self newForSpecification: spec

]

{ #category : 'instance creation' }
RwProjectDefinition class >> newForSpecification: aRwSpecification [

	^ self
		withProperties:
			(Dictionary new
				add: 'name' -> aRwSpecification specName;
				add: 'spec' -> aRwSpecification;
				yourself)
		packageDefinitions: Dictionary new
]

{ #category : 'instance creation' }
RwProjectDefinition class >> withProperties: properties packageDefinitions: packageDefinitions [

	^ self basicNew
		properties: properties;
		packages: packageDefinitions;
		yourself
]

{ #category : 'private' }
RwProjectDefinition >> _compareProperty: propertyKey propertyVaue: propertyValue againstBaseValue: baseValue [

	propertyKey = 'spec' ifFalse: [ ^ super _compareProperty: propertyKey propertyVaue: propertyValue againstBaseValue: baseValue ].
	"spec entries are considered to be equal for comparison purposes"
	^ true

]

{ #category : 'accessing' }
RwProjectDefinition >> addOrUpdatePackage: aPackageDefinition [

	| key |
	key := aPackageDefinition key.
	packages at: key put: aPackageDefinition
]

{ #category : 'accessing' }
RwProjectDefinition >> addPackage: aPackageDefinition [

	| key |
	key := aPackageDefinition key.
	(packages includesKey: key) ifTrue: [self error: 'Duplicate package'].
	^ packages at: key put: aPackageDefinition
]

{ #category : 'accessing' }
RwProjectDefinition >> addPackageNamed: packageName [

	^ self addPackage: (RwPackageDefinition newNamed: packageName)
]

{ #category : 'accessing' }
RwProjectDefinition >> addPackagesNamed: packageNames [

	packageNames
		do: [ :packageName | self addPackage: (RwPackageDefinition newNamed: packageName) ]
]

{ #category : 'properties' }
RwProjectDefinition >> canCommit [

	^ self specification canCommit
]

{ #category : 'properties' }
RwProjectDefinition >> comment [

	^ self specification comment
]

{ #category : 'properties' }
RwProjectDefinition >> comment: aString [

	self specification comment: aString
]

{ #category : 'properties' }
RwProjectDefinition >> configsPath [

	^ self specification configsPath

]

{ #category : 'properties' }
RwProjectDefinition >> configsPath: aDirectoryPathString [

	self specification configsPath: aDirectoryPathString

]

{ #category : 'properties' }
RwProjectDefinition >> configurationExportUrl [

	"Return an RwFileUrl suitable for saving the receiver's configuration in the appropriate configs directory"

	| exportPath |
	self repositoryUrl
		ifNil: [ self error: 'RepositoryUrl has not been set. Receiver has not bee associated with an existing Rowan project on disk.' ].
	exportPath := self repositoryRootPath , '/' , self configsPath , '/'.
	^('file:' , exportPath) asRwUrl

]

{ #category : 'properties' }
RwProjectDefinition >> configurationTemplate [

	"Return a generic RwProjectLoadConfiguration based on the list of packages in the reciever"

	| config groupNames |
	config := RwProjectLoadConfiguration new
		name: 'Default';
		projectName: self specification specName;
		yourself.
	groupNames := self specification defaultGroupNames.
	groupNames
		do: [:groupName | config addDefinedGroupName: groupName ].
	config 
		conditionalPackagesAtConditions: { Rowan platform basePlatformAttribute } 
		andGroup: (groupNames at: 1) 
		addPackageNames: self packageNames.
	^ config

]

{ #category : 'accessing' }
RwProjectDefinition >> definitionWithKey: aKey ifAbsent: absentBlock [

	^packages at: aKey ifAbsent: absentBlock
]

{ #category : 'initialization' }
RwProjectDefinition >> initialize [

	super initialize.
	packages := Dictionary new
]

{ #category : 'testing' }
RwProjectDefinition >> isEmpty [
	"Answer true if this definition does not actually define anything."

	^super isEmpty & packages isEmpty
]

{ #category : 'properties' }
RwProjectDefinition >> key [
	"Answer an object that can be used to uniquely identify myself in the context of my container."

	^self propertyAt: 'name' ifAbsent: [nil]
]

{ #category : 'accessing' }
RwProjectDefinition >> keys [

	^packages keys
]

{ #category : 'properties' }
RwProjectDefinition >> loadedCommitId [

	^ self specification loadedCommitId
]

{ #category : 'properties' }
RwProjectDefinition >> loadedConfigurationNames [

	^ self specification loadedConfigurationNames
]

{ #category : 'accessing' }
RwProjectDefinition >> packageNamed: aString [

	^ self
		packageNamed: aString
		ifAbsent: [ self error: 'A package named ' , aString printString , ' was not found.' ]
]

{ #category : 'accessing' }
RwProjectDefinition >> packageNamed: aString ifAbsent: absentBlock [

	^ self packages at: aString ifAbsent: absentBlock
]

{ #category : 'accessing' }
RwProjectDefinition >> packageNames [
  ^ self packages keys asArray
]

{ #category : 'accessing' }
RwProjectDefinition >> packageNames: packageNames [

	packageNames
		do: [ :packageName | self addPackage: (RwPackageDefinition newNamed: packageName) ]

]

{ #category : 'accessing' }
RwProjectDefinition >> packages [

	^packages
]

{ #category : 'accessing' }
RwProjectDefinition >> packages: aPackageDefinitionDictionary [

	packages := aPackageDefinitionDictionary
]

{ #category : 'copying' }
RwProjectDefinition >> postCopy [

	super postCopy.
	packages := packages copy.
	packages keysAndValuesDo: [:key : value | packages at: key put: value copy ] .
]

{ #category : 'tool api' }
RwProjectDefinition >> readProjectSet [

	^ Rowan projectTools read  readProjectSetForProjectDefinition: self
]

{ #category : 'accessing' }
RwProjectDefinition >> removePackage: aPackageDefinition [

	| key |
	key := aPackageDefinition key.
	(packages includesKey: key) ifFalse: [self error: 'Package not present.'].
	packages removeKey: key

]

{ #category : 'accessing' }
RwProjectDefinition >> removePackageNamed: packageName [

	^self removePackage: (self packageNamed: packageName)

]

{ #category : 'properties' }
RwProjectDefinition >> repoPath [

	^ self specification repoPath
]

{ #category : 'properties' }
RwProjectDefinition >> repoPath: aDirectoryPathString [

	self specification repoPath: aDirectoryPathString

]

{ #category : 'properties' }
RwProjectDefinition >> repositoryRootPath [

	^ self specification repositoryRootPath
]

{ #category : 'properties' }
RwProjectDefinition >> repositoryRootPath: rootRepoPath [

	self specification repositoryRootPath: rootRepoPath
]

{ #category : 'properties' }
RwProjectDefinition >> repositoryUrl [

	^ self specification repositoryUrl
]

{ #category : 'properties' }
RwProjectDefinition >> repositoryUrl: aString [

	self specification repositoryUrl: aString
]

{ #category : 'properties' }
RwProjectDefinition >> specification [

	^ properties at: 'spec'
]

{ #category : 'properties' }
RwProjectDefinition >> specsPath [

	^ self specification specsPath
]

{ #category : 'properties' }
RwProjectDefinition >> specsPath: aDirectoryPathString [

	self specification specsPath: aDirectoryPathString

]

{ #category : 'properties' }
RwProjectDefinition >> useGit [

	^ self specification useGit
]
