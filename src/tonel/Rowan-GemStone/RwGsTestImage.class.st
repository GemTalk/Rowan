Class {
	#name : 'RwGsTestImage',
	#superclass : 'RwGsImage',
	#category : 'Rowan-GemStone'
}

{ #category : 'private' }
RwGsTestImage class >> _loadedProjectRegistryForUserId: aUserId [

	^ self _loadedTestProjectRegistryForUserId: aUserId
]

{ #category : 'private' }
RwGsTestImage class >> _loadedTestProjectRegistryForUserId: aUserId [

	| ug |
	ug := (AllUsers userWithId: aUserId) objectNamed: #'UserGlobals'.
	^ ug
		at: #'RwGsTestLoadedProjectRegistry'
		ifAbsent: [ 
			(self canWrite: ug)
				ifFalse: [ ^ nil ].
			ug at: #'RwGsTestLoadedProjectRegistry' put: StringKeyValueDictionary new ]
]
