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
RwProjectTest >> testSimpleProject [

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
	projectTools write writeSpecification: projectSpec.
	projectTools commit commitSpecification: projectSpec message: 'Initial commit'.
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
		commitSpecification: projectSpec
		message: 'Added Simple class'
]
