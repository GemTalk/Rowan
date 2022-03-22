Class {
	#name : 'RwAbstractRowanSampleTest',
	#superclass : 'RwAbstractV2Test',
	#category : 'Rowan-TestsV2'
}

{ #category : 'private' }
RwAbstractRowanSampleTest class >> _adjustSpecForIssue805: loadSpecification [
	"https://github.com/GemTalk/Rowan/issues/805"

	"have to use https for project url if NOT dhenrich"

	(System gemEnvironmentVariable: 'USER') ~= 'dhenrich'
		ifTrue: [ 
			(loadSpecification projectUrl includesString: 'RowanSample9')
				ifTrue: [ loadSpecification gitUrl: 'https://git@github.com/dalehenrich/RowanSample9' ]
				ifFalse: [ 
					(loadSpecification projectUrl includesString: 'RowanSample10')
						ifTrue: [ loadSpecification gitUrl: 'https://git@github.com/dalehenrich/RowanSample10' ]
						ifFalse: [ self error: 'cannot convert load spec project url to use https://' ] ] ]
]

{ #category : 'private' }
RwAbstractRowanSampleTest class >> _clearLoadSpecSessionCache [
	"self _clearLoadSpecSessionCache"

	SessionTemps current removeKey: self _loadSpecSessionCacheKey ifAbsent: [  ].
	SessionTemps current removeKey: self _gitPullSessionCacheKey ifAbsent: [  ]
]

{ #category : 'private' }
RwAbstractRowanSampleTest class >> _loadSpecNamed: specName [
	"
		self _clearLoadSpecSessionCache
	"

	"The RowanSample9 repository is expected to be 'permanently' located on disk ... 
		we'll use `fetch` and `pull` to update the repo and branches as needed"

	"it is assumed that the load spec on the master branch and the load spec on the branch are the same"

	| loadSpecification resolvedProject resolvedRepository dict theSpec branchDict |
	loadSpecification := self _rowanSample_0000_load_spec.
	theSpec := (SessionTemps current
		at: self _loadSpecSessionCacheKey
		ifAbsent: [  ])
		ifNotNil: [ :specsDict | specsDict at: specName ]
		ifNil: [ 
			"The first time we access the repository in this session, we'll refresh the 
				master branch and cache the session specs"
			loadSpecification revision: 'master'.
			self _adjustSpecForIssue805: loadSpecification.
			resolvedProject := loadSpecification resolveStrict.

			resolvedRepository := resolvedProject repository.
			resolvedRepository
				fetch;
				pull: resolvedRepository remote branch: 'master';
				yourself.

			dict := SessionTemps current
				at: self _loadSpecSessionCacheKey
				ifAbsentPut: [ Dictionary new ].
			(resolvedProject repositoryRoot / 'specs') files
				do: [ :file | 
					file
						readStreamDo: [ :fileStream | 
							| stream spec |
							stream := ZnBufferedReadStream on: fileStream.	"wrap with buffered stream to bypass https://github.com/GemTalk/FileSystemGs/issues/9"
							spec := STON fromStream: stream.
							self _adjustSpecForIssue805: spec.
							dict at: spec specName put: spec ] ].
			dict at: specName ].
	theSpec := theSpec copy.
	theSpec
		projectsHome: self _testRowanProjectsSandbox;
		yourself.
	resolvedProject := theSpec resolveStrict.	"When we reference a spec, we'll checkout the branch"
	branchDict := SessionTemps current
		at: self _gitPullSessionCacheKey
		ifAbsentPut: [ Dictionary new ].
	(branchDict at: theSpec revision ifAbsent: [  ])
		ifNil: [ 
			"Once per session fetch and pull the latest commits for the branch"
			resolvedRepository := resolvedProject repository.
			resolvedRepository
				fetch;
				pull: resolvedRepository remote branch: theSpec revision;
				yourself.
			branchDict at: theSpec revision put: theSpec ].
	^ theSpec copy
]

{ #category : 'private' }
RwAbstractRowanSampleTest class >> _symbolDictionaryNames [
	^ super _symbolDictionaryNames , {'RowanSample9Class1'}
]

{ #category : 'private' }
RwAbstractRowanSampleTest >> _deleteProjectRepositoryDir [
	^ false
]

{ #category : 'private' }
RwAbstractRowanSampleTest >> _gitPullSessionCacheKey [
	^ self class _gitPullSessionCacheKey
]

{ #category : 'private' }
RwAbstractRowanSampleTest >> _loadSpecNamed: specName [
""
	^ self class _loadSpecNamed: specName
]

{ #category : 'private' }
RwAbstractRowanSampleTest >> _loadSpecSessionCacheKey [
	^ self class _loadSpecSessionCacheKey
]

{ #category : 'private' }
RwAbstractRowanSampleTest >> _runTestSuiteForProject: project [
	| testResult testSuite |
	testSuite := project testSuite.
	self deny: testSuite tests isEmpty.
	testResult := testSuite run.
	self deny: testResult hasErrors.
	self deny: testResult hasFailures
]

{ #category : 'private' }
RwAbstractRowanSampleTest >> _standard_resolve_load_validate: projectSpec [
	| resolvedProject loadedProjects |

"resolve/load"
	resolvedProject := projectSpec resolve.
	loadedProjects := resolvedProject load.

	self _standard_validate: resolvedProject loadedProjects: loadedProjects
]

{ #category : 'private' }
RwAbstractRowanSampleTest >> _standard_validate: resolvedProject loadedProjects: loadedProjects [
	| loadedProjectDefinition x |
	"validate"
	self assert: loadedProjects size = 1.
	loadedProjects
		do: [ :project | 
			self assert: (x := project audit) isEmpty.
			self _runTestSuiteForProject: project.
			loadedProjectDefinition := project asDefinition.
			self
				assert:
					loadedProjectDefinition class = resolvedProject projectDefinition class ]
]

{ #category : 'private' }
RwAbstractRowanSampleTest >> _validate: resolvedProject loadedProjects: loadedProjects expectedTestErrors: expectedTestErrors [
	| testResult loadedProjectDefinition x |
	"validate"
	self assert: loadedProjects size = 1.
	loadedProjects
		do: [ :project | 
			| testSuite |
			self assert: (x := project audit) isEmpty.
			testSuite := project testSuite.
			self deny: testSuite tests isEmpty.
			testResult := testSuite run.
			self assert: testResult hasErrors.
			self deny: testResult hasFailures.
			self assert: testResult errors size = expectedTestErrors size.
			self assert: (testResult errors collect: [:case | case selector]) asArray sort = expectedTestErrors sort.
			loadedProjectDefinition := project asDefinition.
			self
				assert:
					loadedProjectDefinition class = resolvedProject projectDefinition class ]
]
