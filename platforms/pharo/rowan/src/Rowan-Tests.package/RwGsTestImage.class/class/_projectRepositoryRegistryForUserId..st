private
_projectRepositoryRegistryForUserId: aUserId

	| ug |
	ug := (AllUsers userWithId: aUserId) objectNamed: #'UserGlobals'.
	^ ug
		at: #'RwGsTestProjectRepositoryRegistry'
		ifAbsent: [ 
			(self currentUserId = aUserId)
				ifFalse: [ ^ nil ].
			ug at: #'RwGsTestProjectRepositoryRegistry' put: StringKeyValueDictionary new ]