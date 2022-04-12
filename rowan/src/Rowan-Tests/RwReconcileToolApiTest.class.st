Class {
	#name : 'RwReconcileToolApiTest',
	#superclass : 'RwBrowserToolTest',
	#category : 'Rowan-Tests'
}

{ #category : 'tests' }
RwReconcileToolApiTest >> testReconcileGlobalExtensionMethods [

	| projectDefinitionSet projectName  packageName1 packageName2 packageName3 projectDefinition 
		classDefinition classExtensionDefinition packageDefinition1 packageDefinition2 packageDefinition3
		className |

	projectName := 'GlobalsExtensionMethods'.
	packageName1 := 'GlobalsExtensionMethods-Core1'.
	packageName2 := 'GlobalsExtensionMethods-Extension1'.
	packageName3 := 'GlobalsExtensionMethods-Extension2'.
	className := 'GlobalsExtensionMethods'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		repositoryRootPath: '/tmp/rowanTest_01/';					"reconcile expects the repo to be on disk"
		repositoryUrl: 'cypress:/tmp/rowanTest_01/rowan/src/';	"reconcile expects the repo to be on disk"
		addPackagesNamed: { packageName1 . packageName2 . packageName3 };
		yourself.

	packageDefinition1 := projectDefinition packageNamed: packageName1.
	packageDefinition2 := projectDefinition packageNamed: packageName2.
	packageDefinition3 := projectDefinition packageNamed: packageName3.

"packageName1 contents"
	classDefinition := (RwClassDefinition
		newForClassNamed: className
			super: 'Object'
			instvars: #(ivar1)
			classinstvars: #()
			classvars: #()
			category: nil
			comment: ''
			pools: #()
			type: 'normal')
		addInstanceMethodDefinition:
			(RwMethodDefinition
				newForSelector: #'foo'
				protocol: 'accessing'
				source: 'foo ^1 ');
		yourself.
	packageDefinition1 
		addClassDefinition: classDefinition.

"packageName2 contents"
	classExtensionDefinition := (RwClassExtensionDefinition newForClassNamed: className)
		addInstanceMethodDefinition:
			(RwMethodDefinition
				newForSelector: #'bar'
				protocol: '*', packageName2 asLowercase
				source: 'bar ^1 ');
		yourself.
	packageDefinition2 
		addClassExtension: classExtensionDefinition.

	classExtensionDefinition := (RwClassExtensionDefinition newForClassNamed: 'Object')
		addInstanceMethodDefinition:
			(RwMethodDefinition
				newForSelector: #'bar'
				protocol: '*', packageName2 asLowercase
				source: 'bar ^1 ');
		addClassMethodDefinition:
			(RwMethodDefinition
				newForSelector: #'bar'
				protocol: '*', packageName2 asLowercase
				source: 'bar ^1 ');
		yourself.
	packageDefinition2 
		addClassExtension: classExtensionDefinition.

"packageName3 contents"

	classExtensionDefinition := (RwClassExtensionDefinition newForClassNamed: 'Object')
		addInstanceMethodDefinition:
			(RwMethodDefinition
				newForSelector: #'foo'
				protocol: '*', packageName3 asLowercase
				source: 'foo ^1 ');
		addClassMethodDefinition:
			(RwMethodDefinition
				newForSelector: #'foo'
				protocol: '*', packageName3 asLowercase
				source: 'foo ^1 ');
		yourself.
	packageDefinition3 
		addClassExtension: classExtensionDefinition.

	classExtensionDefinition := (RwClassExtensionDefinition newForClassNamed: 'ByteArray')
		addInstanceMethodDefinition:
			(RwMethodDefinition
				newForSelector: #'foo'
				protocol: '*', packageName3 asLowercase
				source: 'foo ^1 ');
		yourself.
	packageDefinition3 
		addClassExtension: classExtensionDefinition.

"reconcile"
	projectDefinitionSet := (RwProjectSetDefinition new)
		addProject: projectDefinition;
		yourself.

	Rowan projectTools reconcile
		reconcileGlobalClassExtensionsForProjectDefinitionSet: projectDefinitionSet
		defaultGroupName: 'default' 
		globalsGroupName: 'globals' 
		globalsUserId: 'SystemUser'.

"validate"
]

{ #category : 'tests' }
RwReconcileToolApiTest >> testReconcileGlobalExtensionMethods_issue_290 [

	"https://github.com/dalehenrich/Rowan/issues/290"

	| projectDefinitionSet projectName  packageName1 packageName2 packageName3 projectDefinition 
		classDefinition classExtensionDefinition packageDefinition1 packageDefinition2 packageDefinition3
		className classFileString |

	projectName := 'GlobalsExtensionMethods'.
	packageName1 := 'GlobalsExtensionMethods-Core1'.
	packageName2 := 'GlobalsExtensionMethods-Extension1'.
	packageName3 := 'GlobalsExtensionMethods-Extension2'.
	className := 'GlobalsExtensionMethods'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		repositoryRootPath: '/tmp/rowanTest_02/';					"reconcile expects the repo to be on disk"
		repositoryUrl: 'tonel:/tmp/rowanTest_02/rowan/src/';	"reconcile expects the repo to be on disk"
		addPackagesNamed: { packageName1 . packageName2 . packageName3 };
		yourself.

	packageDefinition1 := projectDefinition packageNamed: packageName1.
	packageDefinition2 := projectDefinition packageNamed: packageName2.
	packageDefinition3 := projectDefinition packageNamed: packageName3.

"packageName1 contents"
	classDefinition := (RwClassDefinition
		newForClassNamed: className
			super: 'Object'
			instvars: #(ivar1)
			classinstvars: #()
			classvars: #()
			category: ''
			comment: ''
			pools: #()
			type: 'normal')
		addInstanceMethodDefinition:
			(RwMethodDefinition
				newForSelector: #'foo'
				protocol: 'accessing'
				source: 'foo ^1 ');
		yourself.
	packageDefinition1 
		addClassDefinition: classDefinition.

"packageName2 contents"
	classExtensionDefinition := (RwClassExtensionDefinition newForClassNamed: className)
		addInstanceMethodDefinition:
			(RwMethodDefinition
				newForSelector: #'bar'
				protocol: '*', packageName2 asLowercase
				source: 'bar ^1 ');
		addClassMethodDefinition:
			(RwMethodDefinition
				newForSelector: #'bar'
				protocol: 'accessing'
				source: 'bar ^1 ');
		yourself.
	packageDefinition2 
		addClassExtension: classExtensionDefinition.

	classExtensionDefinition := (RwClassExtensionDefinition newForClassNamed: 'Object')
		addInstanceMethodDefinition:
			(RwMethodDefinition
				newForSelector: #'bar'
				protocol: '*', packageName2 asLowercase
				source: 'bar ^1 ');
		addClassMethodDefinition:
			(RwMethodDefinition
				newForSelector: #'bar'
				protocol: '*', packageName2 asLowercase
				source: 'bar ^1 ');
		yourself.
	packageDefinition2 
		addClassExtension: classExtensionDefinition.

"packageName3 contents"

	classExtensionDefinition := (RwClassExtensionDefinition newForClassNamed: 'Object')
		addInstanceMethodDefinition:
			(RwMethodDefinition
				newForSelector: #'foo'
				protocol: '*', packageName3 asLowercase
				source: 'foo ^1 ');
		addClassMethodDefinition:
			(RwMethodDefinition
				newForSelector: #'foo'
				protocol: '*', packageName3 asLowercase
				source: 'foo ^1 ');
		yourself.
	packageDefinition3 
		addClassExtension: classExtensionDefinition.

	classExtensionDefinition := (RwClassExtensionDefinition newForClassNamed: 'ByteArray')
		addInstanceMethodDefinition:
			(RwMethodDefinition
				newForSelector: #'foo'
				protocol: '*', packageName3 asLowercase
				source: 'foo ^1 ');
		yourself.
	packageDefinition3 
		addClassExtension: classExtensionDefinition.

"reconcile"
	projectDefinitionSet := (RwProjectSetDefinition new)
		addProject: projectDefinition;
		yourself.

	Rowan projectTools reconcile
		reconcileGlobalClassExtensionsForProjectDefinitionSet: projectDefinitionSet
		defaultGroupName: 'default' 
		globalsGroupName: 'globals' 
		globalsUserId: 'SystemUser'.

	Rowan projectTools write writeProjectDefinition: projectDefinition.
"validate"

	Rowan fileUtilities 
		readStreamFor: '/tmp/rowanTest/rowan/src/GlobalsExtensionMethods-Extension1/GlobalsExtensionMethods.extension.st' 
		do: [:fileStream | classFileString := fileStream contents ].
	self deny: (classFileString includesString: 'classSide')
]
