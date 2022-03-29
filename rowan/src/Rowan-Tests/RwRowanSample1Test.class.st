Class {
	#name : 'RwRowanSample1Test',
	#superclass : 'RwToolTest',
	#category : 'Rowan-Tests'
}

{ #category : 'private' }
RwRowanSample1Test >> _rowanSample1LoadSpecificationUrl [

	| rowanSpec |
	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	^ 'file:' , rowanSpec repositoryRootPath , '/samples/RowanSample1.ston'

]

{ #category : 'tests' }
RwRowanSample1Test >> testIssue345 [

	| specUrlString projectTools rowanSpec gitTool gitRootPath projectName rowanSampleSpec symDict registry |
	projectName := 'RowanSample1'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :project | Rowan image _removeLoadedProject: project ].

	rowanSpec := (Rowan image _projectForNonTestProject: 'Rowan') specification.
	specUrlString := self _rowanSample1LoadSpecificationUrl.
	projectTools := Rowan projectTools.

	gitRootPath := rowanSpec repositoryRootPath , '/test/testRepositories/repos/'.

	(Rowan fileUtilities directoryExists: gitRootPath , projectName)
		ifTrue: [ Rowan fileUtilities deleteAll: gitRootPath , projectName ].

	projectTools clone
		cloneSpecUrl: specUrlString
		gitRootPath: rowanSpec repositoryRootPath , '/test/testRepositories/repos/'
		useSsh: false.

	rowanSampleSpec := (Rowan image loadedProjectNamed: projectName) specification.
	(rowanSampleSpec platformSpec at: 'gemstone')
		projectOwnerId: Rowan image currentUserId;
		defaultSymbolDictName: self _symbolDictionaryName;
		yourself.

	gitRootPath := rowanSampleSpec repositoryRootPath.

	gitTool := projectTools git.
	gitTool gitcheckoutIn: gitRootPath with: 'issue_345'.
"load project"
	projectTools load loadProjectNamed: projectName withConfiguration: 'Default'.

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
