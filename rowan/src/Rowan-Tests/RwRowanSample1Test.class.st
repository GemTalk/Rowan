Class {
	#name : 'RwRowanSample1Test',
	#superclass : 'RwRowanSampleAbstractTest',
	#category : 'Rowan-Tests'
}

{ #category : 'tests' }
RwRowanSample1Test >> testIssue345 [

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
