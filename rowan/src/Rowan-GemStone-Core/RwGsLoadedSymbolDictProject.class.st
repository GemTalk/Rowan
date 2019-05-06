Class {
	#name : 'RwGsLoadedSymbolDictProject',
	#superclass : 'RwLoadedProject',
	#category : 'Rowan-GemStone-Core'
}

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
RwGsLoadedSymbolDictProject >> repositoryRoot [

	^ self specification repositoryRootPath
		ifNotNil: [:path | path asFileReference ]
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
