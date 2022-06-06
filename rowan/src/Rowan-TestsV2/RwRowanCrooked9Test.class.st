Class {
	#name : 'RwRowanCrooked9Test',
	#superclass : 'RwAbstractRowanSampleTest',
	#category : 'Rowan-TestsV2'
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

	| loadSpec projectName rowanSample9Project crookedProjectName crookedSourceProjectName |
	loadSpec := RwRowanSample9Test _loadSpecNamed: 'spec_0082'.
	projectName := loadSpec projectName.
	crookedProjectName := 'RowanCrooked9_gen'.
	crookedSourceProjectName := 'RowanCrooked9'.

	{projectName.
	crookedProjectName}
		do: [ :pName | 
			(Rowan image loadedProjectNamed: pName ifAbsent: [  ])
				ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].

	rowanSample9Project := loadSpec resolve load.

	self _checkoutBranchNamed: 'spec_0003'.

	{'Rowan'.
	'RowanHybrid'.
	'Monticello'}
		do: [ :packageConvention | 
			| resolvedProject definedProject |
			definedProject := (Rowan newProjectNamed: crookedProjectName)
				packageConvention: packageConvention;
				gemstoneSetDefaultSymbolDictNameTo: self _sampleSymbolDictionaryName1;
				repoType: #'git';
				packageFormat: 'topaz';
				projectsHome: self _testRowanProjectsSandbox / crookedProjectName;
				addTopLevelComponentNamed: 'Core';
				yourself.
			(Rowan globalNamed: 'GsFileinPackager')
				toPackagesForDefinedProject: definedProject
				fromServerPath:
					(self _testRowanProjectsSandbox / crookedSourceProjectName / 'extensions.gs') pathString.
self halt: 'that stupid cross resolve boundary reread from disk bug!!!!'.
			resolvedProject := definedProject resolve.
			resolvedProject write.
self halt ]
]
