accessing
resetTestProjectRegistry

	"RwGsTestImage resetTestProjectRegistry"

	"drop any projects that may be present test project registry"

	((AllUsers userWithId: self currentUserId) objectNamed: #'UserGlobals')
		removeKey: #'RwGsTestLoadedProjectRegistry' ifAbsent: [  ];
		removeKey: #'RwGsTestProjectRepositoryRegistry' ifAbsent: [  ];
		yourself