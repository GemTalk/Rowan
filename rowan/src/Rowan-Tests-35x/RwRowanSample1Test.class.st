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

	| specUrlString projectTools rowanProject gitTool gitRootPath projectName project symDict registry |
	projectName := 'RowanSample1'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	specUrlString := self _rowanSample1LoadSpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := rowanProject repositoryRootPath asFileReference / 'test/testRepositories/repos/'.

	(gitRootPath / projectName) ensureDeleteAll.

	self 
		_cloneProjectFromSpecUrl: specUrlString 
		projectsHome: gitRootPath.

	project := RwProject newNamed: projectName.
	project
		defaultSymbolDictName: self _symbolDictionaryName;
		yourself.

	gitRootPath := project repositoryRootPath asFileReference.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: gitRootPath with: self _issue_345_branch_name.
"load project"
	project load.

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
