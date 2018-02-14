Class {
	#name : 'RwProjectDefinition',
	#superclass : 'RwDefinition',
	#instVars : [
		'packages'
	],
	#category : 'Rowan-Core'
}

{ #category : 'instance creation' }
RwProjectDefinition class >> newForGitMetacelloProjectNamed: aString [

	| spec |
	spec := RwMetacelloProjectSpecification new
		specName: aString;
		yourself.
	^ self newForSpecification: spec
]

{ #category : 'instance creation' }
RwProjectDefinition class >> newForGitPackageProjectNamed: aString [

	| spec |
	spec := RwPackageProjectSpecification new
		specName: aString;
		yourself.
	^ self newForSpecification: spec
]

{ #category : 'instance creation' }
RwProjectDefinition class >> newForSpecification: aRwSpecification [

	^ self new
		properties:
				(Dictionary new
						add: 'name' -> aRwSpecification specName;
						add: 'spec' -> aRwSpecification;
						yourself);
		yourself
]

{ #category : 'instance creation' }
RwProjectDefinition class >> withProperties: properties packageDefinitions: packageDefinitions [

	^ self basicNew
		properties: properties;
		packages: packageDefinitions;
		yourself
]

{ #category : 'accessing' }
RwProjectDefinition >> addPackage: aPackageDefinition [

	| key |
	key := aPackageDefinition key.
	(packages includesKey: key) ifTrue: [self error: 'Duplicate package'].
	packages at: key put: aPackageDefinition
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

{ #category : 'comparing' }
RwProjectDefinition >> compareAgainstBase: aDefinition [

	| modification packagesModification |
	modification := RwProjectModification before: aDefinition after: self.
	modification
		propertiesModification: (self comparePropertiesAgainstBase: aDefinition).
	packagesModification := RwPackagesModification new.
	self
		compareDictionary: packages
		againstBaseDictionary: aDefinition packages
		into: packagesModification
		elementClass: RwPackageDefinition.
	modification packagesModification: packagesModification.
	^ modification
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
RwProjectDefinition >> loadedPackageNames [

	^ self specification loadedPackageNames
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

{ #category : 'properties' }
RwProjectDefinition >> packageNamesInLoadOrder [

	^ self specification packageNamesInLoadOrder
]

{ #category : 'properties' }
RwProjectDefinition >> packageNamesInLoadOrder: packageNamesInLoadOrder [

	packageNamesInLoadOrder
		do: [ :packageName | self addPackage: (RwPackageDefinition newNamed: packageName) ].
	self specification packageNamesInLoadOrder: packageNamesInLoadOrder
]

{ #category : 'accessing' }
RwProjectDefinition >> packages [

	^packages
]

{ #category : 'accessing' }
RwProjectDefinition >> packages: aPackageDefinitionDictionary [

	packages := aPackageDefinitionDictionary
]

{ #category : 'printing' }
RwProjectDefinition >> printOn: aStream [

	aStream nextPutAll: self asString
]

{ #category : 'properties' }
RwProjectDefinition >> repoPath [

	^ self specification repoPath
]

{ #category : 'properties' }
RwProjectDefinition >> repoPath: repoPath [

	self specification repoPath: repoPath
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
RwProjectDefinition >> useGit [

	^ self specification useGit
]
