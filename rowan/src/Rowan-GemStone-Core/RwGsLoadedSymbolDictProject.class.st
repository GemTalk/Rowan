Class {
	#name : 'RwGsLoadedSymbolDictProject',
	#superclass : 'RwLoadedProject',
	#category : 'Rowan-GemStone-Core'
}

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

{ #category : 'accessing' }
RwGsLoadedSymbolDictComponentProject >> useGit [

	^ self specification useGit
]
