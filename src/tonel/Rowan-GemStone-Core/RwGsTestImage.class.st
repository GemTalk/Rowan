Class {
	#name : 'RwGsTestImage',
	#superclass : 'RwGsImage',
	#category : 'Rowan-GemStone'
}

{ #category : 'querying' }
RwGsTestImage class >> _loadedProjectNamedForNonTestProjects: aString ifAbsent: absentBlock [

	"Look up a loaded project. First in current users project registry, then scan AllUsers for the project ... note that the user needs to be the project owner."

	| currentUserId |
	currentUserId := self currentUserId.
	^ self _loadedProjectRegistry
		at: aString
		ifAbsent: [ 
			AllUsers
				do: [ :userProfile | 
					| aUserId |
					aUserId := userProfile userId.
					aUserId ~= currentUserId
						ifTrue: [ 
							(self _loadedProjectRegistryForUserId: aUserId)
								ifNotNil: [ :projectRegistry | 
									(projectRegistry at: aString ifAbsent: [  ])
										ifNotNil: [ :loadedProject | 
											(loadedProject specification platformSpec at: 'gemstone') projectOwnerId
												= aUserId
												ifTrue: [ ^ loadedProject ] ] ] ] ].
			^ absentBlock value ]

]

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

{ #category : 'accessing' }
RwGsTestImage class >> _projectForNonTestProject: projectName [

	" this is a method that should only need to be used by tests ... and then sparingly"

	^self _loadedProjectNamedForNonTestProjects: projectName ifAbsent: [self error: 'no project ', projectName printString, ' found.']

]

{ #category : 'accessing' }
RwGsTestImage class >> resetTestProjectRegistry [

	"self resetTestProjectRegistry"

	"drop any projects that may be present test project registry"

	((AllUsers userWithId: self currentUserId) objectNamed: #'UserGlobals')
		removeKey: #'RwGsTestLoadedProjectRegistry'
		ifAbsent: [  ]
]
