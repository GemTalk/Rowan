Class {
	#name : 'RwGsLoadedSymbolDictProject',
	#superclass : 'RwLoadedProject',
	#category : 'Rowan-GemStone-Core'
}

{ #category : 'testing' }
RwGsLoadedSymbolDictProject >> canCommit [

	^ self specification canCommit
]

{ #category : 'commit log' }
RwGsLoadedSymbolDictProject >> commitLog: logLimit [

	self useGit ifFalse: [ ^ '' ].
	^ Rowan gitTools gitlogtool: 'HEAD' limit: logLimit gitRepoDirectory: self repositoryRootPath
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictProject >> loadedCommitId [

	^ self specification loadedCommitId
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictProject >> loadedConfigurationNames: configNames [

	self specification imageSpec loadedConfigurationNames: configNames
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictProject >> loadedGroupNames: groupNames [

	self specification imageSpec loadedGroupNames: groupNames
]

{ #category : 'specifiction' }
RwGsLoadedSymbolDictProject >> projectOwnerId [

	^ (self specification platformSpec at: 'gemstone') projectOwnerId
]

{ #category : 'specifiction' }
RwGsLoadedSymbolDictProject >> projectOwnerId: aUserId [

	^ (self specification platformSpec at: 'gemstone') projectOwnerId: aUserId

]

{ #category : 'definitions' }
RwGsLoadedSymbolDictProject >> propertiesForDefinition [

	| props |
	props := super propertiesForDefinition.
	props at: 'spec' put: handle.
	^ props
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictProject >> repositoryCommitId [

	| rootPath |
	self useGit
		ifFalse: [ ^ '' ].
	rootPath := self repositoryRootPath.
	rootPath ifNil: [ ^ '' ].
	^ Rowan gitTools gitcommitShaIn: rootPath
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictProject >> repositoryRoot [
	"Root directory of the project. The configsPath, repoPath, specsPath, and projectsPath are specified relative to the repository root."

	^ self repositoryRootPath
		ifNotNil: [:path | path asFileReference ]
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictProject >> repositoryRootPath [

	^ self specification repositoryRootPath
]

{ #category : 'properties' }
RwGsLoadedSymbolDictProject >> specification [

	^ handle
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictProject >> symbolDictNameForPackageNamed: packageName [

	| spec gemstoneSpec |
	spec := self specification.
	gemstoneSpec := spec platformSpec at: 'gemstone'.
	^ gemstoneSpec symbolDictNameForPackageNamed: packageName.
]

{ #category : 'loading' }
RwGsLoadedSymbolDictProject >> updateLoadedCommitId [

	| loadedCommitId |
	self useGit ifFalse: [ ^ nil ].
	loadedCommitId := [  Rowan gitTools gitcommitShaIn: self repositoryRoot pathString ]
		on: Error
		do: [ :ignored | 
			"most likely no commits yet"
			^ nil ].
	self specification imageSpec loadedCommitId: loadedCommitId
]

{ #category : 'accessing' }
RwGsLoadedSymbolDictProject >> useGit [

	^ self specification useGit
]
