Class {
	#name : 'RwRowanCrooked9Test',
	#superclass : 'RwAbstractRowanSampleTest',
	#category : 'Rowan-TestsV3'
}

{ #category : 'private' }
RwRowanCrooked9Test class >> _branchSessionCacheKey [
	^ #'RowanCrooked9BranchesSet'
]

{ #category : 'private' }
RwRowanCrooked9Test class >> _checkoutBranchNamed: branchName [
	"
		self _clearLoadSpecSessionCache
	"

	"The RowanCrooked9 repository is expected to be 'permanently' located on disk ... 
		we'll use `fetch` and `pull` to update the repo and branches as needed"

	"this method will clone the project (if necessary) and checkout the given branch"

	| gitTools branchPulledDict gitRoot repositoryRoot |
	gitTools := Rowan gitTools.
	gitRoot := self _testRowanProjectsSandbox.
	repositoryRoot :=  gitRoot / 'RowanCrooked9'.
	(SessionTemps current at: self _branchSessionCacheKey ifAbsent: [  ])
		ifNotNil: [ 
			"repository has been cloned"
			 ]
		ifNil: [ 
			repositoryRoot exists
				ifFalse: [ 
					"clone the project"
					gitTools
						gitcloneIn: gitRoot pathString
						with: 'git@github.com:dalehenrich/RowanCrooked9.git' ] ].
	branchPulledDict := SessionTemps current
		at: self _gitPullSessionCacheKey
		ifAbsentPut: [ Dictionary new ].
	gitTools gitcheckoutIn: repositoryRoot with: branchName.
	(branchPulledDict at: branchName ifAbsent: [  ])
		ifNil: [ 
			"Once per session fetch and pull the latest commits for the branch"
			gitTools
				gitfetchIn: repositoryRoot with: 'origin';
				gitpullIn: repositoryRoot pathString remote: 'origin' branch: branchName;
				yourself.
			branchPulledDict at: branchName put: true ].
	^ branchName

]

{ #category : 'private' }
RwRowanCrooked9Test class >> _gitPullSessionCacheKey [
	^ #'RowanCrooked9GitBranchDict'
]

{ #category : 'private' }
RwRowanCrooked9Test class >> _loadSpecSessionCacheKey [
	^ self _branchSessionCacheKey
]

{ #category : 'private' }
RwRowanCrooked9Test >> _checkoutBranchNamed: branchName [
	^ self class _checkoutBranchNamed: branchName
]

{ #category : 'tests' }
RwRowanCrooked9Test >> testIssue820 [
	"https://github.com/GemTalk/Rowan/issues/820"

	| loadSpec projectName rowanSample9Project crookedProjectName crookedSourceProjectName rowanProjectsSandbox |
	loadSpec := RwRowanSample9V3Test _loadSpecNamed: 'spec_0082'.
	projectName := loadSpec projectName.
	crookedProjectName := 'RowanCrooked9_gen'.
	crookedSourceProjectName := 'RowanCrooked9'.

	{projectName.
	(crookedProjectName , '_Rowan').
	(crookedProjectName , '_RowanHybrid').
	(crookedProjectName , '_Monticello')}
		do: [ :pName | 
			(Rowan image loadedProjectNamed: pName ifAbsent: [  ])
				ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

	rowanSample9Project := loadSpec resolve load.

	self _checkoutBranchNamed: 'spec_0004'.

	rowanProjectsSandbox := self _testRowanProjectsSandbox.
	{'Rowan'.
	'RowanHybrid'.
	'Monticello'}
		do: [ :packageConvention | 
			| resolvedProject definedProject theProjectName |
			theProjectName := crookedProjectName , '_' , packageConvention.
			(rowanProjectsSandbox / theProjectName) ensureDeleteAll.
			definedProject := (Rowan newProjectNamed: theProjectName)
				packageConvention: packageConvention;
				gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
				repoType: #'git';
				packageFormat: 'topaz';
				projectsHome: rowanProjectsSandbox;
				addLoadComponentNamed: 'Core';
				yourself.
			(Rowan globalNamed: 'GsFileinPackager')
				toPackagesForDefinedProject: definedProject
				fromServerPath:
					(rowanProjectsSandbox / crookedSourceProjectName / 'extensions.gs') pathString.
			resolvedProject := definedProject resolveProject.
			resolvedProject write ].

	{'Rowan'.
	'RowanHybrid'.
	'Monticello'}
		do: [ :packageConvention | 
			| theProjectName loadedProject testResult audit |
			"now load/test/unload each project in turn"
			theProjectName := crookedProjectName , '_' , packageConvention.
			loadedProject := (Rowan
				projectFromUrl:
					'file:'
						,
							(rowanProjectsSandbox / theProjectName / 'rowan' / 'specs' / theProjectName
								, 'ston') pathString
				projectsHome: rowanProjectsSandbox) load projectNamed: theProjectName.
			packageConvention ~= 'Monticello'
				ifTrue: [ 
					"audit for Monticello packaage convention not yet supported (https://github.com/GemTalk/Rowan/issues/829)"
					self assert: (audit := loadedProject audit) isEmpty ].
			testResult := loadedProject testSuite run.
			self deny: testResult hasErrors.
			self deny: testResult hasFailures.
			loadedProject unload ]
]

{ #category : 'tests' }
RwRowanCrooked9Test >> testIssue896_1 [
	"https://github.com/GemTalk/Rowan/issues/896 -- filetree duplicates"

	| loadSpec crookedProject crookedProjectName  |
	self _checkoutBranchNamed: 'spec_0005'.
	crookedProjectName := 'RowanCrooked9'.

	{crookedProjectName.}
		do: [ :pName | 
			(Rowan image loadedProjectNamed: pName ifAbsent: [  ])
				ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].
	
	loadSpec := RwSpecification fromUrl: 'file:', (self _testRowanProjectsSandbox / crookedProjectName / 'rowan' / 'specs' / 'spec_0005.ston') fullName.
	[ crookedProject := loadSpec resolve load ]
		on: Error
		do: [:ex | self halt ].
	self assert: false description: 'should not get error ... expected error'
]
