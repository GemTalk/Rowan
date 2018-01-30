Class {
	#name : 'RwProjectTest',
	#superclass : 'RwAbstractTest',
	#category : 'Rowan-Tode'
}

{ #category : 'private' }
RwProjectTest >> _symbolDictionaryName [

	^ #'RowanTestData'
]

{ #category : 'private' }
RwProjectTest >> gsInteractionConfirmationHandler [

	^ GsInteractionHandler new
		defaultBlock: [ :ignored | self assert: false description: 'expected a confirmation' ];
		confirmBlock: [ :interaction | interaction ok ];
		yourself
]

{ #category : 'private' }
RwProjectTest >> gsInteractionInformFailureHandler [

	^ GsInteractionHandler new
		defaultBlock: [ :ignored | self assert: false description: 'unexpected internaction' ];
		informBlock: [ :interaction | self assert: false description: 'unexpected inform' ];
		yourself
]

{ #category : 'private' }
RwProjectTest >> handleConfirmationDuring: aBlock [

	"expect a confirmation"

	aBlock
		on: GsInteractionRequest
		do: [ :ex | 
			ex
				response:
					(ex interaction interactWith: self gsInteractionConfirmationHandler) ]
]

{ #category : 'private' }
RwProjectTest >> handleInformAsFailureDuring: aBlock [

	aBlock
		on: GsInteractionRequest
		do: [ :ex | 
			ex
				response:
					(ex interaction interactWith: self gsInteractionInformFailureHandler) ]
]

{ #category : 'tests' }
RwProjectTest >> testProjectClassExtensions [

	"Build our project in memory without committing to disk until we've created a class with methods, then write to disk."

	| projectName projectSpec projectTools classDefinition packageDefinition1 packageDefinition2 className testClass testInstance classExtensionDefinition |
	projectName := 'Simple'.
	projectTools := Rowan projectTools.
	(Rowan projectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :spec | projectTools unregister unregisterSpecification: spec ].

	projectSpec := projectTools spec newPackageSpecNamed: projectName useGit: true.
	projectSpec
		comment:
				'This is a simple project created in memory first, then written to disk.';
		packageNames: #('Simple-Core' 'Simple-Extensions' 'Simple-Tests');
		yourself.
	(projectSpec platformSpec at: 'gemstone')
		symbolDictName: self _symbolDictionaryName.

	className := 'Simple'.
	classDefinition := RwClassDefinition
		newForClassNamed: className
		super: 'Object'
		instvars: #('ivar1')
		classinstvars: #(#'civar1')
		classvars: #()
		category: nil
		comment: 'I am a Simple class with extensions'
		pools: #()
		type: 'normal'.

	packageDefinition1 := RwPackageDefinition newNamed: 'Simple-Core'.
	packageDefinition1 addClassDefinition: classDefinition.
	projectTools load
		loadPackageDefinition: packageDefinition1
		specification: projectSpec.

	classExtensionDefinition := RwClassExtensionDefinition
		newForClassNamed: className.
	classExtensionDefinition
		addInstanceMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'ivar1'
						protocol: 'accessing'
						source: 'ivar1 ^ivar1');
		addClassMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'initialize'
						protocol: 'initialization'
						source: 'initialize civar1 := 1.');
		addClassMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'civar1'
						protocol: 'accessing'
						source: 'civar1 ^civar1');
		yourself.

	packageDefinition2 := RwPackageDefinition newNamed: 'Simple-Extensions'.
	packageDefinition2 addClassExtension: classExtensionDefinition.
	projectTools load
		loadPackageDefinition: packageDefinition2
		specification: projectSpec.

	testClass := Rowan globalNamed: className.
	self assert: testClass notNil.
	self assert: testClass civar1 == 1.
	testInstance := testClass new.
	self assert: testInstance ivar1 isNil.

	self
		handleConfirmationDuring: [ 
			projectTools create
				createPackageProjectFor: projectSpec
				format: 'tonel'
				root: '/tmp/rowanSimpleExtensionProject/'
				repoPath: 'src' ].
	projectTools spec exportSpecification: projectSpec.
	projectTools write writePackagesForSpecification: projectSpec.
	projectTools commit
		commitPackagesForSpecification: projectSpec
		message: 'Added Simple class and extension methods'
]

{ #category : 'tests' }
RwProjectTest >> testSimpleProject1 [

	"Create project and build disk-based artifacts first, then create create a class and write changes to disk."

	| projectName projectSpec projectTools classDefinition packageSymbolDictionary packageDefinition packageSet |
	projectName := 'Simple'.
	projectTools := Rowan projectTools.
	(Rowan projectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :spec | projectTools unregister unregisterSpecification: spec ].
	self
		handleConfirmationDuring: [ 
			projectSpec := projectTools create
				createPackageProject: projectName
				format: 'tonel'
				root: '/tmp/rowanSimpleProject/'
				repoPath: 'src'
				useGit: true ].
	projectSpec
		comment:
				'This is a simple project to demonstrate the smalltalk API used for a project lifecycle';
		packageNames: #('Simple-Core' 'Simple-Tests');
		yourself.
	(projectSpec platformSpec at: 'gemstone')
		symbolDictName: self _symbolDictionaryName.
	packageSymbolDictionary := self
		_newOrExistingPackageSymbolDictionaryNamed: self _symbolDictionaryName.
	projectTools spec exportSpecification: projectSpec.
	projectTools write writePackagesForSpecification: projectSpec.
	projectTools commit
		commitPackagesForSpecification: projectSpec
		message: 'Initial commit'.
	self
		handleInformAsFailureDuring: [ projectTools load loadSpecification: projectSpec ].

	classDefinition := RwClassDefinition
		newForClassNamed: 'Simple'
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: nil
		comment: 'I am a Simple class'
		pools: #()
		type: 'normal'.

	packageDefinition := packageSymbolDictionary
		loadedPackageDefinitionNamed: 'Simple-Core'
		ifAbsent: [ self assert: false description: 'expected to find loaded package' ].
	packageDefinition addClassDefinition: classDefinition.

	self
		handleInformAsFailureDuring: [ 
			projectTools load
				loadPackageDefinition: packageDefinition
				specification: projectSpec ].

	projectTools spec exportSpecification: projectSpec.
	projectTools write writePackagesForSpecification: projectSpec.
	projectTools commit
		commitPackagesForSpecification: projectSpec
		message: 'Added Simple class'
]

{ #category : 'tests' }
RwProjectTest >> testSimpleProject2 [

	"Build our project in memory without committing to disk until we've created a class, then write to disk."

	| projectName projectSpec projectTools classDefinition packageDefinition packageSet |
	projectName := 'Simple'.
	projectTools := Rowan projectTools.
	(Rowan projectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :spec | projectTools unregister unregisterSpecification: spec ].

	projectSpec := projectTools spec newPackageSpecNamed: projectName useGit: true.
	projectSpec
		comment:
				'This is a simple project created in memory first, then written to disk.';
		packageNames: #('Simple-Core' 'Simple-Tests');
		yourself.
	(projectSpec platformSpec at: 'gemstone')
		symbolDictName: self _symbolDictionaryName.

	classDefinition := RwClassDefinition
		newForClassNamed: 'Simple2'
		super: 'Object'
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: nil
		comment: 'I am a Simple2 class'
		pools: #()
		type: 'normal'.

	packageDefinition := RwPackageDefinition newNamed: 'Simple-Core'.
	packageDefinition addClassDefinition: classDefinition.

	projectTools load
		loadPackageDefinition: packageDefinition
		specification: projectSpec.

	self
		handleConfirmationDuring: [ 
			projectTools create
				createPackageProjectFor: projectSpec
				format: 'tonel'
				root: '/tmp/rowanSimpleProject2/'
				repoPath: 'src' ].
	projectTools spec exportSpecification: projectSpec.
	projectTools write writePackagesForSpecification: projectSpec.
	projectTools commit
		commitPackagesForSpecification: projectSpec
		message: 'Added Simple class'
]

{ #category : 'tests' }
RwProjectTest >> testSimpleProject3 [

	"Build our project in memory without committing to disk until we've created a class with methods, then write to disk."

	| projectName projectSpec projectTools classDefinition packageDefinition packageSet className testClass testInstance |
	projectName := 'Simple'.
	projectTools := Rowan projectTools.
	(Rowan projectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :spec | projectTools unregister unregisterSpecification: spec ].

	projectSpec := projectTools spec newPackageSpecNamed: projectName useGit: true.
	projectSpec
		comment:
				'This is a simple project created in memory first, then written to disk.';
		packageNames: #('Simple-Core' 'Simple-Tests');
		yourself.
	(projectSpec platformSpec at: 'gemstone')
		symbolDictName: self _symbolDictionaryName.

	className := 'Simple3'.
	classDefinition := RwClassDefinition
		newForClassNamed: className
		super: 'Object'
		instvars: #('ivar1')
		classinstvars: #(#'civar1')
		classvars: #()
		category: nil
		comment: 'I am a Simple3 class'
		pools: #()
		type: 'normal'.

	classDefinition
		addInstanceMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'ivar1'
						protocol: 'accessing'
						source: 'ivar1 ^ivar1');
		addClassMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'initialize'
						protocol: 'initialization'
						source: 'initialize civar1 := 1.');
		addClassMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'civar1'
						protocol: 'accessing'
						source: 'civar1 ^civar1');
		yourself.

	packageDefinition := RwPackageDefinition newNamed: 'Simple-Core'.
	packageDefinition addClassDefinition: classDefinition.

	projectTools load
		loadPackageDefinition: packageDefinition
		specification: projectSpec.

	testClass := Rowan globalNamed: className.
	self assert: testClass notNil.
	self assert: testClass civar1 == 1.
	testInstance := testClass new.
	self assert: testInstance ivar1 isNil.

	self
		handleConfirmationDuring: [ 
			projectTools create
				createPackageProjectFor: projectSpec
				format: 'tonel'
				root: '/tmp/rowanSimpleProject3/'
				repoPath: 'src' ].
	projectTools spec exportSpecification: projectSpec.
	projectTools write writePackagesForSpecification: projectSpec.
	projectTools commit
		commitPackagesForSpecification: projectSpec
		message: 'Added Simple3 class and methods'
]
