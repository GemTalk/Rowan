Class {
	#name : 'RwRowanSample1Test',
	#superclass : 'RwRowanSampleAbstractTest',
	#category : 'Rowan-Tests-35x'
}

{ #category : 'private' }
RwRowanSample1Test >> _issue_345_branch_name [

	^ 'issue_345_v2'
]

{ #category : 'private' }
RwRowanSample1Test >> _rowanSample1LoadSpecificationUrl [

	| rowanProject |
	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	^ 'file:' , rowanProject repositoryRootPath , '/samples/RowanSample1_v2.ston'
]

{ #category : 'tests' }
RwRowanSample1Test >> testIssue345 [

	"https://github.com/GemTalk/Rowan/issues/514"

	"This test (for 3.5.0) is actually about being able to change the component
		properties before loading ... post clone ... Right now I am not completely
		sure what sematics should be applied:
			- disallow changing component properteis - if you want different properties
				edit the component --- NOT COOL
			- somehow arrange for changes made to the ""loaded project"" to survive
				the read that ??must happend?? before being able to load the project 
				from disk
			- require that the project be read from disk before making changes and
				then somehow arranging to do a load of the project in memory and avoid
				re-reading from disk ... THIS PROBABLY THE RIGHT ANSWER ... need to 
				get second and third opinions on this one ... and expand test coverage,
				I assume that there are existing tests (the extension methods in this 
				package are good clues) that have hacked some sort of behavior in this
				area, but we need a REAL solution with some logic behind it more than
				""this is how it works:)"""

	| specUrlString projectTools rowanProject gitTool gitRootPath projectName projectDef 
		symDict registry |
	projectName := 'RowanSample1'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	specUrlString := self _rowanSample1LoadSpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := rowanProject repositoryRootPath asFileReference / 'test/testRepositories/repos/'.

	(gitRootPath / projectName) ensureDeleteAll.

	projectDef := self 
		_cloneProjectFromSpecUrl: specUrlString 
		projectsHome: gitRootPath
		registerProject: false.

	projectDef
		defaultSymbolDictName: self _symbolDictionaryName;
		yourself.

	gitRootPath := projectDef repositoryRootPath asFileReference.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: gitRootPath with: self _issue_345_branch_name.

"load project"
	projectDef load.

"validation"
	self assert: (Rowan image loadedProjectNamed: projectName ifAbsent: []) notNil.

	symDict := Rowan globalNamed: self _symbolDictionaryName.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 3.
	self assert: registry classRegistry size = 2.
	self assert: registry classExtensionRegistry size = 1.
	self assert: registry methodRegistry size = 6.

"delete project"
	Rowan projectTools delete deleteProjectNamed: projectName.

"validation"
	self assert: (Rowan image loadedProjectNamed: projectName ifAbsent: []) isNil.

	symDict := Rowan globalNamed: self _symbolDictionaryName.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 0.
	self assert: registry classRegistry size = 0.
	self assert: registry classExtensionRegistry size = 0.
	self assert: registry methodRegistry size = 0.
]

{ #category : 'tests' }
RwRowanSample1Test >> testIssue514 [

	"https://github.com/GemTalk/Rowan/issues/514"

	"Explicitly confirm that the property change was made and propogated to loaded project ... 
		hmmm and loaded project needs to be dirty because it doesn't match disk"

	| specUrlString projectTools rowanProject gitRootPath projectName projectDef propertyModValidationBlock
		loadedProjectDefinition diskProjectSet imageProjectSet projectSetModification project |
	projectName := 'RowanSample1'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	specUrlString := self _rowanSample1LoadSpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := rowanProject repositoryRootPath asFileReference / 'test/testRepositories/repos/'.

	(gitRootPath / projectName) ensureDeleteAll.

	projectDef := self 
		_cloneProjectFromSpecUrl: specUrlString 
		projectsHome: gitRootPath
		registerProject: false.

	projectDef
		defaultSymbolDictName: self _symbolDictionaryName;
		yourself.

"load project"
	projectDef load.

"validation"
	self assert: (Rowan image loadedProjectNamed: projectName ifAbsent: []) notNil.

	self assert: (Rowan globalNamed: self _symbolDictionaryName) notNil.

	project := Rowan projectNamed: projectName.
	self assert: project isDirty.	"since project not loaded from source that came from disk, it should be dirty"

"compare"
	loadedProjectDefinition := project asDefinition.
	diskProjectSet :=  loadedProjectDefinition copy readProjectSet.
	imageProjectSet := RwProjectSetDefinition new
		addProject: loadedProjectDefinition;
		yourself.
	projectSetModification :=  imageProjectSet compareAgainstBase: diskProjectSet. "new vs. old"
	self deny: projectSetModification isEmpty.

"validate that property changes are correct"
	propertyModValidationBlock := [ :propertiesModification | 
		"property modification should have a new value for gs_SymbolDictionary equal to <self _symbolDictionaryName>"
		| propertyElementsModified propertyModification |
		self deny: propertiesModification isEmpty.
		propertyElementsModified := propertiesModification elementsModified.
		self assert: propertyElementsModified size = 1.
		propertyModification := propertyElementsModified at: 'gs_SymbolDictionary'.
		self assert: propertyModification oldValue isNil.
		self assert: propertyModification newValue = self _symbolDictionaryName asString ].
	(RwSample1TestValidationVisitor new)
		defaultNoopBlock: [ self assert: false description: 'unexpected modification' ];
		changedProjectBlock: [:projectModification | self assert: projectModification propertiesModification isEmpty ];
		changedPackageBlock: [:packageModification | 
			| packageName |
			packageName := packageModification key.
			(#('RowanSample1-Extensions' 'RowanSample1-Core' 'RowanSample1-Tests' ) includes: packageName)
				ifTrue: [ 
					packageName = 'RowanSample1-Extensions'
						ifTrue: [ self assert: packageModification classesModification isEmpty ]
						ifFalse: [ self deny: packageModification classesModification isEmpty ].
					self assert: packageModification classExtensionsModification isEmpty.
					propertyModValidationBlock value: packageModification propertiesModification ]
				ifFalse: [
					self assert: packageModification propertiesModification isEmpty ] ];
		changedClassBlock: [ :classModification :visitor |
			| packageName |
			packageName := visitor currentPackageDefinition key.
			self assert: (#('RowanSample1-Core' 'RowanSample1-Tests' ) includes: packageName).
			self assert: classModification instanceMethodsModification isEmpty.
			self assert: classModification classMethodsModification isEmpty.
			propertyModValidationBlock value: classModification propertiesModification ];
		visit: projectSetModification.
]
