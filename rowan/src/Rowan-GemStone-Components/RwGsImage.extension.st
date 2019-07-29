Extension { #name : 'RwGsImage' }

{ #category : '*rowan-gemstone-components' }
RwGsImage class >> _projectRepositoryRegistry [

	^ self _projectRepositoryRegistryForUserId: self currentUserId
]

{ #category : '*rowan-gemstone-components' }
RwGsImage class >> _projectRepositoryRegistryForUserId: aUserId [

	| userProfile userPlatformDict |
	userProfile := AllUsers userWithId: aUserId.
	(userProfile defaultObjectSecurityPolicy ifNil: [true] ifNotNil: [:x |x currentUserCanRead])
		ifFalse: [
			"do not have permissions to read objects created by <aUserId>"
			^ nil ].
	userPlatformDict := RwPlatform _userPlatformDictionaryForUser: aUserId.
	^ userPlatformDict
		at: #'RwGsProjectRepositoryRegistry'
		ifAbsent: [ 
			(self currentUserId = aUserId)
				ifFalse: [ ^ nil ].
			userPlatformDict at: #'RwGsProjectRepositoryRegistry' put: StringKeyValueDictionary new ]
]

{ #category : '*rowan-gemstone-components' }
RwGsImage class >> _removeProjectRepository:  aRwProjectRepository [

	"note that it is not safe to just remove a project repository from the registry, however this method is supplied as a convenience for tests"

	| repositoryName repositoryRegistry |
	repositoryName := aRwProjectRepository name.
	repositoryRegistry := self _projectRepositoryRegistry.
	(repositoryRegistry at: repositoryName ifAbsent: [ ^ nil ]) == aRwProjectRepository
		ifFalse: [ 
			self
				error:
					'The specified project repository is not identical to the project repository in the registry ... this is not expected' ].
	repositoryRegistry removeKey: repositoryName
]

{ #category : '*rowan-gemstone-components' }
RwGsImage class >> addProjectRepository: newProjectRepository [

	"Register a repository"

	| repositoryName projectRepositoryRegistry |
	repositoryName := newProjectRepository name.
	projectRepositoryRegistry := self
		_projectRepositoryRegistry.
	projectRepositoryRegistry
		at: repositoryName
		ifAbsent: [ ^ projectRepositoryRegistry at: repositoryName put: newProjectRepository ].
	self
		error:
			'There is already a project repository named ' , repositoryName printString , ' registered'
]

{ #category : '*rowan-gemstone-components' }
RwGsImage class >> projectRepositoryNamed: aString [

	^ self
		projectRepositoryNamed: aString
		ifAbsent: [ self error: 'No project repository named ' , aString printString , ' found' ]
]

{ #category : '*rowan-gemstone-components' }
RwGsImage class >> projectRepositoryNamed: aString ifAbsent: absentBlock [

	"Look up a project repository."

	^ self _projectRepositoryRegistry
		at: aString
		ifAbsent: absentBlock
]
