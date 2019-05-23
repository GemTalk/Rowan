private
_loadedTestProjectRegistryForUserId: aUserId

	| ug |
	ug := (AllUsers userWithId: aUserId) objectNamed: #'UserGlobals'.
	^ ug
		at: #'RwGsTestLoadedProjectRegistry'
		ifAbsent: [ 
			(self currentUserId = aUserId)
				ifFalse: [ ^ nil ].
			ug at: #'RwGsTestLoadedProjectRegistry' put: StringKeyValueDictionary new ]
