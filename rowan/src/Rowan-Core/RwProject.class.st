Class {
	#name : 'RwProject',
	#superclass : 'RowanInterface',
	#category : 'Rowan-Core'
}

{ #category : 'private' }
RwProject >> _loadedProject [

	^ Rowan image loadedProjectNamed: self name
]

{ #category : 'private' }
RwProject >> _specification [

	^ self _loadedProject specification
]

{ #category : 'properties' }
RwProject >> canCommit [

	^ self _specification canCommit
]

{ #category : 'properties' }
RwProject >> comment [

	^ self _specification comment
]

{ #category : 'accessing' }
RwProject >> currentBranchName [

	| rootPath |
	self useGit
		ifFalse: [ ^ '' ].
	rootPath := self repositoryRootPath.
	rootPath ifNil: [ ^ '' ].
	^ Rowan gitTools gitBranchNameIn:rootPath

]

{ #category : 'accessing' }
RwProject >> definedClasses [

	^ self _projectTools query classForProjectNamed: self name
]

{ #category : 'testing' }
RwProject >> existsOnDisk [

	Rowan image loadedProjectNamed: self name ifAbsent: [ ^false ].
	^ Rowan platform fileUtilities directoryExists: self repositoryRootPath.
]

{ #category : 'accessing' }
RwProject >> extendedClasses [

	^ self _projectTools query classExtensionsForProjectNamed: self name
]

{ #category : 'testing' }
RwProject >> isDirty [

	^self _loadedProject isDirty

]

{ #category : 'properties' }
RwProject >> loadedCommitId [

	^ self _specification loadedCommitId
]

{ #category : 'accessing' }
RwProject >> packageNames [

	^ self _loadedProject packageNames
]

{ #category : 'accessing' }
RwProject >> packages [

	^ self packageNames collect: [ :packageName | RwPackage newNamed: packageName ]
]

{ #category : 'accessing' }
RwProject >> project [

	^ self
]

{ #category : 'project creation' }
RwProject >> register [
	"Create a loaded project based on the receiver's properties in the image."

	self _specification register

]

{ #category : 'properties' }
RwProject >> remote [

	^ self _specification remote
]

{ #category : 'accessing' }
RwProject >> repositoryCommitId [

	| rootPath |
	self useGit
		ifFalse: [ ^ '' ].
	rootPath := self repositoryRootPath.
	rootPath ifNil: [ ^ '' ].
	^ Rowan gitTools gitcommitShaIn: rootPath

]

{ #category : 'properties' }
RwProject >> repositoryRootPath [

	^ self _specification repositoryRootPath
]

{ #category : 'testing' }
RwProject >> useGit [

	^self _specification useGit

]
