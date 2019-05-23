tests
testCreateComponentProject

	"Excercise RwComponentProjectDefinition instance creation protocol ... 
		create the project on disk -- this is the full deal"

	| projectUrl projectName configurationNames groupNames comment projectHome
		rowanSpec cpd x y packageName className testClassName|

	projectName := 'RowanSample7_component'.
	configurationNames := #( 'Main' ).
	groupNames := #( 'core'  'tests').
	projectUrl := self _gitHubProjectUrl.
	comment := 'test project reference'.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].
	(Rowan image projectRepositoryNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :repo | Rowan image _removeProjectRepository: repo ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	projectHome := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

"create project definition"
	cpd := RwComponentProjectDefinition
		projectName: projectName 
			configurationNames: configurationNames 
			groupNames: groupNames 
			useGit: true 
			projectUrl: projectUrl 
			comment: comment.

"create package definitions"
	cpd
		addPackageNamed: projectName, '-Extension' 
			toComponentNamed: 'Main' 
			withConditions: #( 'common' ) 
			andGroupName: 'core';
		addPackageNamed: projectName, '-Core' 
			toComponentNamed: 'Main' 
			withConditions: #( 'common' ) 
			andGroupName: 'core';
		addPackageNamed: projectName, '-GemStone-Core' 
			toComponentNamed: 'Main' 
			withConditions: #( 'gemstone' ) 
			andGroupName: 'core';
		addPackageNamed: projectName, '-Tests_Main' 
			toComponentNamed: 'Main' 
			withConditions: #( 'common' ) 
			andGroupName: 'tests';
		addPackageNamed: projectName, '-Tests-GemStone_Main' 
			toComponentNamed: 'Main' 
			withConditions: #( 'gemstone' ) 
			andGroupName: 'tests';
		yourself.

"create class and method definitions"
	packageName := projectName, '-Core'.
	className := projectName, 'Core'.
	((cpd packageNamed: packageName)
		addClassNamed: className
			super: 'Object' 
			category: packageName)
		addInstanceMethod: 'foo ^''self foo_platform''' protocol: '*', packageName asLowercase.

	packageName := projectName, '-Extension'.
	((cpd packageNamed: packageName) addClassExtensionNamed: className)
		addClassMethod: 'foo ^''foo''' protocol: '*', packageName asLowercase.

	packageName := projectName, '-GemStone-Core'.
	((cpd packageNamed: packageName) addClassExtensionNamed: className)
		addInstanceMethod: 'foo_platform ^''foo_gemstone''' protocol: '*', packageName asLowercase.

	packageName := projectName, '-Tests_Main'.
	testClassName := projectName, 'Tests_Main'.
	((cpd packageNamed: packageName)
		addClassNamed: testClassName
			super: 'TestCase' 
			category: packageName)
		addInstanceMethod: 'test1 self assert: ', className, ' new foo = self _platform_response' protocol: '*', packageName asLowercase.

	packageName := projectName, '-Tests-GemStone_Main'.
	((cpd packageNamed: packageName) addClassExtensionNamed:testClassName)
		addInstanceMethod: '_platform_response ^ ''foo_gemstone''' protocol: '*', packageName asLowercase.

"prepare to export component project definition"
	cpd projectHome: projectHome.
	cpd repositoryRoot ensureDeleteAll.

"create component project on disk"
	cpd create.

"validate"
	self assert: cpd repositoryRoot exists.

	self assert: cpd configsRoot exists.
	self assert: (cpd configsRoot / (configurationNames at: 1) , 'ston') exists.
	self assert: (x := (cpd configsRoot / (configurationNames at: 1) , 'ston') readStream contents) = ( y:= self _expected_rowanSample7_component_configuration).

	self assert: cpd specsRoot exists.
	self assert: (cpd specsRoot / projectName , 'ston') exists.
	self assert: (x := (cpd specsRoot / projectName , 'ston') readStream contents) = ( y:= self _expected_rowanSample7_component_specification).

	self assert: cpd packagesRoot exists.
	self assert: (cpd packagesRoot /  'properties' , 'st') exists.
	self assert: (x := (cpd packagesRoot /  'properties' , 'st') readStream contents) = ( y:= self _expected_rowanSample7_project_st).
	{ projectName, '-GemStone-Core'. projectName, '-Core' . projectName, '-Tests-GemStone_Main'. 
		projectName, '-Extension' . projectName, '-Tests_Main' . projectName, '-Tests-GemStone_Main'
	}
		do: [:pName | 
			| packageDir |
			packageDir := cpd packageFormat = 'tonel'
				ifTrue: [ cpd packagesRoot / pName ]
				ifFalse: [ cpd packagesRoot / pName , 'package' ].
			self assert: packageDir exists ].

	self assert: cpd projectsRoot exists.

	self assert: (Rowan image loadedProjectNamed: projectName ifAbsent: [  ]) notNil.
	self assert: (Rowan image projectRepositoryNamed: projectName ifAbsent: [  ]) notNil