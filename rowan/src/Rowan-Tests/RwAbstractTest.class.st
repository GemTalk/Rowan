Class {
	#name : 'RwAbstractTest',
	#superclass : 'TestCase',
	#category : 'Rowan-Tests'
}

{ #category : 'private' }
RwAbstractTest class >> _symbolDictionaryName [

	^ #'CypressTestData'
]

{ #category : 'private' }
RwAbstractTest class >> _symbolDictionaryName1 [

	^ #'RowanTestData'
]

{ #category : 'private' }
RwAbstractTest class >> _symbolDictionaryName2 [

	^ #'RowanTestData2'
]

{ #category : 'private' }
RwAbstractTest class >> _symbolDictionaryNames [

	^ 	{(self _symbolDictionaryName).
		(self _symbolDictionaryName1).
		(self _symbolDictionaryName2).
		(self _unmanagedDictionaryName)}

]

{ #category : 'private' }
RwAbstractTest class >> _testRowanProjectsSandbox [
	"working repository for use as ROWAN_PROJECTS_HOME for tests"

	| rowanProject |
	(System gemEnvironmentVariable: 'ROWAN_PROJECTS_SANDBOX')
		ifNotNil: [ :sandboxPath | ^ sandboxPath asFileReference ].
	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	^ rowanProject repositoryRoot / 'test/testRepositories/repos/'
]

{ #category : 'private' }
RwAbstractTest class >> _unmanagedDictionaryName [

	^ #'UnmanagedPackages'
]

{ #category : 'cleanup' }
RwAbstractTest class >> cleanup [
	"self cleanup"

	| user symListP symListT dictP dictT index |
	user := System myUserProfile.
	symListP := user symbolList.
	symListT := GsCurrentSession currentSession symbolList.
	self _symbolDictionaryNames
		do: [ :symDictName | 
			dictP := symListP
				detect: [ :each | (each at: symDictName ifAbsent: [ nil ]) == each ]
				ifNone: [  ].
			dictP
				ifNotNil: [ 
					(dictP at: GsPackagePolicy globalName otherwise: nil)
						ifNotNil: [ :policy | policy disable ].
					index := symListP indexOf: dictP.
					index > 0
						ifTrue: [ user removeDictionaryAt: index ] ].
			dictT := symListT
				detect: [ :each | (each at: symDictName ifAbsent: [ nil ]) == each ]
				ifNone: [  ].
			dictT
				ifNotNil: [ 
					(dictT at: GsPackagePolicy globalName otherwise: nil)
						ifNotNil: [ :policy | policy disable ].
					index := symListT indexOf: dictT.
					index > 0
						ifTrue: [ symListT removeAtIndex: index ] ] ].
	GsPackagePolicy current refreshSessionMethodDictionary.
	ClassOrganizer clearCachedOrganizer
]

{ #category : 'private' }
RwAbstractTest >> _classvars [

	^ #('ClassVar1')
]

{ #category : 'private' }
RwAbstractTest >> _loadedPackageDefinitionsNamed: someNames [

	"Answer a PackageSetDefinition containing packages with the given names in the image.
 	If any name is not a loaded package, that name is ignored."

	^ (self _loadedPackagesNamed: someNames) asPackageDefinitionSet

]

{ #category : 'private' }
RwAbstractTest >> _loadedPackagesNamed: someNames [

	"Answer a PackageSet containing packages with the given names in the image.
 	If any name is not a loaded package, that name is ignored."

	"very likely we should be dealing with a project name 
at this point ... unless there is a use case for loading a collection of packages without directly using the project interface --- loaded packages do know what project they belong to ... so there might be something done behind the scenes"

	| loadedPackages |
	loadedPackages := someNames
		collect: [ :aName | Rowan image loadedPackageNamed: aName ifAbsent: [ nil ] ].
	^ RwEntitySet withAll: (loadedPackages select: [ :each | each notNil ])

]

{ #category : 'private' }
RwAbstractTest >> _loadedProjectsNamed: someNames [

	"Answer a RwProjectSetDefinition containing projects with the given names in the image.
 	If any name is not a loaded project, that name is ignored."

	| loadedProjects |
	loadedProjects := someNames
		collect: [ :aName | Rowan image loadedProjectNamed: aName ifAbsent: [ nil ] ].
	^ RwEntitySet withAll: (loadedProjects select: [ :each | each notNil ])

]

{ #category : 'private' }
RwAbstractTest >> _symbolDictionary [

	^ Rowan image newOrExistingSymbolDictionaryNamed: self _symbolDictionaryName
]

{ #category : 'private' }
RwAbstractTest >> _symbolDictionaryName [

	^ self class _symbolDictionaryName
]

{ #category : 'private' }
RwAbstractTest >> _symbolDictionaryName1 [

	^ self class _symbolDictionaryName1
]

{ #category : 'private' }
RwAbstractTest >> _symbolDictionaryName2 [

	^ self class _symbolDictionaryName2
]

{ #category : 'private' }
RwAbstractTest >> _testRowanProjectsSandbox [
	"working repository for use as ROWAN_PROJECTS_HOME for tests"

	^ self class _testRowanProjectsSandbox
]

{ #category : 'private' }
RwAbstractTest >> _unmanagedDictionaryName [

	^ self class _unmanagedDictionaryName
]

{ #category : 'support' }
RwAbstractTest >> assertAbsenceOfPackage: packageName [

	| loadedPackage symbolDict |
	symbolDict := Rowan image
		newOrExistingSymbolDictionaryNamed: self _symbolDictionaryName.
	loadedPackage := symbolDict rowanSymbolDictionaryRegistry
		loadedPackageNamed: packageName
		ifAbsent: [  ].
	self assert: loadedPackage equals: nil
]

{ #category : 'support' }
RwAbstractTest >> classDefinition: className super: superClassName category: category inPackage: packageName [

	^ Dictionary new
		at: className
			put:
				(RwClassDefinition
						withProperties:
							(self
								propertiesForClassNamed: className
								super: superClassName
								category: category
								inPackage: packageName)
						classMethodDefinitions: Dictionary new
						instanceMethodDefinitions: Dictionary new);
		yourself
]

{ #category : 'support' }
RwAbstractTest >> classDefinition: className super: superClassName classVars: classVars inPackage: packageName [

	^ Dictionary new
		at: className
			put:
				(RwClassDefinition
						withProperties:
							(self
								propertiesForClassNamed: className
								super: superClassName
								classVars: classVars
								inPackage: packageName)
						classMethodDefinitions: Dictionary new
						instanceMethodDefinitions: Dictionary new);
		yourself
]

{ #category : 'support' }
RwAbstractTest >> classDefinition: className super: superClassName comment: comment inPackage: packageName [

	^ Dictionary new
		at: className
			put:
				(RwClassDefinition
						withProperties:
							(self
								propertiesForClassNamed: className
								super: superClassName
								comment: comment
								inPackage: packageName)
						classMethodDefinitions: Dictionary new
						instanceMethodDefinitions: Dictionary new);
		yourself
]

{ #category : 'support' }
RwAbstractTest >> classDefinition: className super: superClassName inPackage: packageName [

	^ Dictionary new
		at: className
			put:
				(RwClassDefinition
						withProperties:
							(self
								propertiesForClassNamed: className
								super: superClassName
								inPackage: packageName)
						classMethodDefinitions: Dictionary new
						instanceMethodDefinitions: Dictionary new);
		yourself
]

{ #category : 'support' }
RwAbstractTest >> classDefinition: className super: superClassName instanceMethods: instanceMethods classMethods: classMethods inPackage: packageName [

	^ Dictionary new
		at: className
			put:
				(RwClassDefinition
						withProperties:
							(self
								propertiesForClassNamed: className
								super: superClassName
								inPackage: packageName)
						classMethodDefinitions: classMethods
						instanceMethodDefinitions: instanceMethods);
		yourself
]

{ #category : 'support' }
RwAbstractTest >> classDefinition: className super: superClassName instVars: instVars inPackage: packageName [

	^ Dictionary new
		at: className
			put:
				(RwClassDefinition
						withProperties:
							(self
								propertiesForClassNamed: className
								super: superClassName
								instVars: instVars
								inPackage: packageName)
						classMethodDefinitions: Dictionary new
						instanceMethodDefinitions: Dictionary new);
		yourself
]

{ #category : 'support' }
RwAbstractTest >> classExtensionDefinition: className instanceMethods: instanceMethods classMethods: classMethods [

	^ Dictionary new
		at: className
			put:
				(RwClassExtensionDefinition
						withProperties: (self propertiesForClassExtensionNamed: className)
						classMethodDefinitions: classMethods
						instanceMethodDefinitions: instanceMethods);
		yourself
]

{ #category : 'private' }
RwAbstractTest >> gsInteractionConfirmationHandler [

	^ GsInteractionHandler new
		defaultBlock: [ :ignored | self assert: false description: 'expected a confirmation' ];
		confirmBlock: [ :interaction | interaction ok ];
		yourself
]

{ #category : 'private' }
RwAbstractTest >> gsInteractionInformHandler: interactionBlock [

	^ GsInteractionHandler new
		defaultBlock: [ :ignored | self assert: false description: 'expected an inform' ];
		informBlock: interactionBlock;
		yourself
]

{ #category : 'private' }
RwAbstractTest >> handleConfirmationDuring: aBlock [

	"expect a confirmation"

	aBlock
		on: GsInteractionRequest
		do: [ :ex | 
			ex
				response:
					(ex interaction interactWith: self gsInteractionConfirmationHandler) ]
]

{ #category : 'private' }
RwAbstractTest >> handleInformDuring: aBlock interactionBlock: interactionBlock [

	"expect a confirmation"

	aBlock
		on: GsInteractionRequest
		do: [ :ex | 
			ex
				response:
					(ex interaction interactWith: (self gsInteractionInformHandler: interactionBlock)) ]
]

{ #category : 'support' }
RwAbstractTest >> methodDefsFromSpec: specArray [
  | dict |
  dict := Dictionary new.
  specArray
    do: [ :spec | 
      | selector protocol source |
      selector := (spec at: 1) asSymbol.
      protocol := (spec at: 2) asString.
      source := (spec at: 3) asString.
      dict
        at: selector
        put:
          (RwMethodDefinition
            withProperties:
              (SymbolDictionary with: #'selector' -> selector with: #'protocol' -> protocol)
            source: source) ].
  ^ dict
]

{ #category : 'support' }
RwAbstractTest >> packageDefinition: packageName [

	^ RwPackageDefinition
		withProperties: (SymbolDictionary with: #'name' -> packageName  with: #'gs_SymbolDictionary' -> self _symbolDictionaryName asString)
		classDefinitions: Dictionary new
		classExtensions: Dictionary new
		traitDefinitions: Dictionary new
]

{ #category : 'support' }
RwAbstractTest >> packageDefinition: packageName classNamed: className super: superClassName [

	^ RwPackageDefinition
		withProperties: (SymbolDictionary with: #'name' -> packageName  with: #'gs_SymbolDictionary' -> self _symbolDictionaryName asString)
		classDefinitions:
			(self classDefinition: className super: superClassName inPackage: packageName)
		classExtensions: Dictionary new
		traitDefinitions: Dictionary new
]

{ #category : 'support' }
RwAbstractTest >> packageDefinition: packageName classNamed: className super: superClassName category: category [

	^ RwPackageDefinition
		withProperties: (SymbolDictionary with: #'name' -> packageName  with: #'gs_SymbolDictionary' -> self _symbolDictionaryName asString)
		classDefinitions:
			(self
				classDefinition: className
				super: superClassName
				category: category
				inPackage: packageName)
		classExtensions: Dictionary new
		traitDefinitions: Dictionary new
]

{ #category : 'support' }
RwAbstractTest >> packageDefinition: packageName classNamed: className super: superClassName classVars: classVars [

	^ RwPackageDefinition
		withProperties: (SymbolDictionary with: #'name' -> packageName  with: #'gs_SymbolDictionary' -> self _symbolDictionaryName asString)
		classDefinitions:
			(self
				classDefinition: className
				super: superClassName
				classVars: classVars
				inPackage: packageName)
		classExtensions: Dictionary new
		traitDefinitions: Dictionary new
]

{ #category : 'support' }
RwAbstractTest >> packageDefinition: packageName classNamed: className super: superClassName comment: comment [

	^ RwPackageDefinition
		withProperties: (SymbolDictionary with: #'name' -> packageName  with: #'gs_SymbolDictionary' -> self _symbolDictionaryName asString)
		classDefinitions:
			(self
				classDefinition: className
				super: superClassName
				comment: comment
				inPackage: packageName)
		classExtensions: Dictionary new
		traitDefinitions: Dictionary new
]

{ #category : 'support' }
RwAbstractTest >> packageDefinition: packageName classNamed: className super: superClassName instanceMethods: instanceMethods classMethods: classMethods [

	^ RwPackageDefinition
		withProperties: (SymbolDictionary with: #'name' -> packageName  with: #'gs_SymbolDictionary' -> self _symbolDictionaryName asString)
		classDefinitions:
			(self
				classDefinition: className
				super: superClassName
				instanceMethods: instanceMethods
				classMethods: classMethods
				inPackage: packageName)
		classExtensions: Dictionary new
		traitDefinitions: Dictionary new
]

{ #category : 'support' }
RwAbstractTest >> packageDefinition: packageName classNamed: className super: superClassName instVars: instVars [

	^ RwPackageDefinition
		withProperties: (SymbolDictionary with: #'name' -> packageName  with: #'gs_SymbolDictionary' -> self _symbolDictionaryName asString)
		classDefinitions:
			(self
				classDefinition: className
				super: superClassName
				instVars: instVars
				inPackage: packageName)
		classExtensions: Dictionary new
		traitDefinitions: Dictionary new
]

{ #category : 'support' }
RwAbstractTest >> packageDefinition: packageName extendClassNamed: className instanceMethods: instanceMethods classMethods: classMethods [

	^ RwPackageDefinition
		withProperties: (SymbolDictionary with: #'name' -> packageName  with: #'gs_SymbolDictionary' -> self _symbolDictionaryName asString)
		classDefinitions: Dictionary new
		classExtensions:
			(self
				classExtensionDefinition: className
				instanceMethods: instanceMethods
				classMethods: classMethods)
		traitDefinitions: Dictionary new
]

{ #category : 'support' }
RwAbstractTest >> packageSetDefinition: packageName [

	^ (RwPackageSetDefinition new addPackage: (self packageDefinition: packageName))
		asDefinition
]

{ #category : 'support' }
RwAbstractTest >> packageSetDefinition: packageName classNamed: className super: superClassName [

	^ (RwPackageSetDefinition new
		addPackage:
			(self packageDefinition: packageName classNamed: className super: superClassName))
		asDefinition
]

{ #category : 'support' }
RwAbstractTest >> packageSetDefinition: packageName classNamed: className super: superClassName category: category [

	^ (RwPackageSetDefinition new
		addPackage:
			(self
				packageDefinition: packageName
				classNamed: className
				super: superClassName
				category: category))
		asDefinition
]

{ #category : 'support' }
RwAbstractTest >> packageSetDefinition: packageName classNamed: className super: superClassName classVars: classVars [

	^ (RwPackageSetDefinition new
		addPackage:
			(self
				packageDefinition: packageName
				classNamed: className
				super: superClassName
				classVars: classVars))
		asDefinition
]

{ #category : 'support' }
RwAbstractTest >> packageSetDefinition: packageName classNamed: className super: superClassName comment: comment [

	^ (RwPackageSetDefinition new
		addPackage:
			(self
				packageDefinition: packageName
				classNamed: className
				super: superClassName
				comment: comment))
		asDefinition
]

{ #category : 'support' }
RwAbstractTest >> packageSetDefinition: packageName classNamed: className super: superClassName instanceMethods: instanceMethods classMethods: classMethods [

	^ (RwPackageSetDefinition new
		addPackage:
			(self
				packageDefinition: packageName
				classNamed: className
				super: superClassName
				instanceMethods: instanceMethods
				classMethods: classMethods))
		asDefinition
]

{ #category : 'support' }
RwAbstractTest >> packageSetDefinition: packageName classNamed: className super: superClassName instVars: instVars [

	^ (RwPackageSetDefinition new
		addPackage:
			(self
				packageDefinition: packageName
				classNamed: className
				super: superClassName
				instVars: instVars))
		asDefinition
]

{ #category : 'support' }
RwAbstractTest >> packageSetDefinition: packageName extendClassNamed: className instanceMethods: instanceMethods classMethods: classMethods [

	^ (RwPackageSetDefinition new
		addPackage:
			(self
				packageDefinition: packageName
				extendClassNamed: className
				instanceMethods: instanceMethods
				classMethods: classMethods))
		asDefinition
]

{ #category : 'support' }
RwAbstractTest >> propertiesForClassExtensionNamed: className [

	| dict |
	dict := SymbolDictionary new.
	dict
		at: #'name' put: className .
	^ dict
]

{ #category : 'support' }
RwAbstractTest >> propertiesForClassNamed: className super: superClassName category: category inPackage: packageName [

	^ self
		propertiesForClassNamed: className
		super: superClassName
		instvars: {}
		classinstvars: {}
		classvars: {}
		category: category
		comment: nil
		gs_SymbolDictionary: self _symbolDictionaryName asString
		pools: {}
		type: 'normal'
		gs_options: {}
		inPackage: packageName
]

{ #category : 'support' }
RwAbstractTest >> propertiesForClassNamed: className super: superClassName classVars: classVars inPackage: packageName [

	^ self
		propertiesForClassNamed: className
		super: superClassName
		instvars: {}
		classinstvars: {}
		classvars: classVars
		category: nil
		comment: nil
		gs_SymbolDictionary: self _symbolDictionaryName asString
		pools: {}
		type: 'normal'
		gs_options: {}
		inPackage: packageName
]

{ #category : 'support' }
RwAbstractTest >> propertiesForClassNamed: className super: superClassName comment: comment inPackage: packageName [

	^ self
		propertiesForClassNamed: className
		super: superClassName
		instvars: {}
		classinstvars: {}
		classvars: {}
		category: nil
		comment: comment
		gs_SymbolDictionary: self _symbolDictionaryName asString
		pools: {}
		type: 'normal'
		gs_options: {}
		inPackage: packageName
]

{ #category : 'support' }
RwAbstractTest >> propertiesForClassNamed: className super: superClassName inPackage: packageName [

	^ self
		propertiesForClassNamed: className
		super: superClassName
		instvars: {}
		classinstvars: {}
		classvars: {}
		category: nil
		comment: nil
		gs_SymbolDictionary: self _symbolDictionaryName asString
		pools: {}
		type: 'normal'
		gs_options: {}
		inPackage: packageName
]

{ #category : 'support' }
RwAbstractTest >> propertiesForClassNamed: className super: superClassName instvars: instvars classinstvars: classinstvars classvars: classvars category: category comment: comment gs_SymbolDictionary: gs_SymbolDictionary pools: pools type: type gs_options: gs_options inPackage: packageName [

	| dict |
	dict := SymbolDictionary new.
	category
		ifNotNil: [ 
			dict
				at: #'category' put: category ].
	dict
		at: #'classinstvars' put: classinstvars;
		at: #'classvars' put: classvars;
		at: #'gs_SymbolDictionary' put: gs_SymbolDictionary;
		at: #'instvars' put: instvars;
		at: #'name' put: className;
		at: #'pools' put: pools;
		at: #'superclass' put: superClassName;
		at: #'type' put: type.
	comment ifNotNil: [ dict at: #'comment' put: comment ].
	(gs_options _isArray and: [ gs_options isEmpty not ])
		ifTrue: [ 
			dict
				at: #'gs_options'
				put:
					(gs_options collect: [ :each | each asString ]) asSortedCollection asArray ].
	^ dict
]

{ #category : 'support' }
RwAbstractTest >> propertiesForClassNamed: className super: superClassName instvars: instvars classinstvars: classinstvars classvars: classvars comment: comment gs_SymbolDictionary: gs_SymbolDictionary pools: pools type: type gs_options: gs_options inPackage: packageName [

	^ self
		propertiesForClassNamed: className
		super: superClassName
		instvars: instvars
		classinstvars: classinstvars
		classvars: classvars
		category: packageName
		comment: comment
		gs_SymbolDictionary: gs_SymbolDictionary
		pools: pools
		type: type
		gs_options: gs_options
		inPackage: packageName
]

{ #category : 'support' }
RwAbstractTest >> propertiesForClassNamed: className super: superClassName instVars: instVars inPackage: packageName [

	^ self
		propertiesForClassNamed: className
		super: superClassName
		instvars: instVars
		classinstvars: {}
		classvars: {}
		category: nil
		comment: nil
		gs_SymbolDictionary: self _symbolDictionaryName asString
		pools: {}
		type: 'normal'
		gs_options: {}
		inPackage: packageName
]

{ #category : 'running' }
RwAbstractTest >> setUp [
	super setUp.
	self class cleanup.
	Rowan platform _alternateImageClass: Rowan image testImageClass
]

{ #category : 'running' }
RwAbstractTest >> tearDown [
	super tearDown.
	Rowan platform _alternateImageClass: nil.
	self class cleanup
]
