Class {
	#name : 'RwRowanSample9Test',
	#superclass : 'RwAbstractV2Test',
	#category : 'Rowan-TestsV2'
}

{ #category : 'private' }
RwRowanSample9Test class >> _clearLoadSpecSessionCache [
	"self _clearLoadSpecSessionCache"

	SessionTemps current removeKey: self _loadSpecSessionCacheKey ifAbsent: [  ]
]

{ #category : 'private' }
RwRowanSample9Test class >> _loadSpecSessionCacheKey [
	^ #'RowanSample9SpecsDict'
]

{ #category : 'private' }
RwRowanSample9Test >> _deleteProjectRepositoryDir [
	^ false
]

{ #category : 'issue_527' }
RwRowanSample9Test >> _issue_527_resolve_load_validate: loadSpec className: className expectedSymDictName: expectedSymDictName [
	| projectDefinition loadedProjects symDictName symDict |
	projectDefinition := loadSpec resolve.
	loadedProjects := projectDefinition load.
	self assert: loadedProjects size = 1.
	loadedProjects do: [ :project | self assert: project audit isEmpty ].

	symDictName := loadSpec
		gemstoneDefaultSymbolDictNameForUser: Rowan image currentUserId.

	self assert: symDictName = expectedSymDictName.

	(symDict := Rowan globalNamed: symDictName)
		ifNil: [ 
			self
				assert: false
				description:
					'Expected symbol dictionary ' , symDictName printString , ' not present' ].

	self assert: (symDict at: className asSymbol ifAbsent: [  ]) notNil.

	(loadedProjects at: 1) unload.

	self deny: (symDict at: className asSymbol ifAbsent: [  ]) notNil
]

{ #category : 'private' }
RwRowanSample9Test >> _loadSpecNamed: specName [
	"The RowanSample9 repository is expected to be 'permanently' located on disk ... 
		we'll use `fetch` and `pull` to update the repo and branches as needed"

	"it is assumed that the load spec on the master branch and the load spec on the branch are the same"

	| loadSpecification repositoryDefinition dict theSpec |
	loadSpecification := self _rowanSample9_0000_load_spec.
	theSpec := (SessionTemps current at: self _loadSpecSessionCacheKey ifAbsent: [  ])
		ifNotNil: [ :specsDict | specsDict at: specName ]
		ifNil: [ 
			"The first time we access the repository in this session, we'll refresh the 
				master branch and cache the session specs"
			loadSpecification revision: 'master'.
			loadSpecification resolveStrict.

			repositoryDefinition := loadSpecification repositoryDefinition.
			repositoryDefinition
				fetch;
				pull: repositoryDefinition remote branch: 'master';
				yourself.

			dict := SessionTemps current
				at: self _loadSpecSessionCacheKey
				ifAbsentPut: [ Dictionary new ].
			(loadSpecification repositoryRoot / 'specs') files
				do: [ :file | 
					file
						readStreamDo: [ :fileStream | 
							| stream spec |
							stream := ZnBufferedReadStream on: fileStream.	"wrap with buffered stream to bypass https://github.com/GemTalk/FileSystemGs/issues/9"
							spec := STON fromStream: stream.
							dict at: spec specName put: spec ] ].
			dict at: specName ].
	theSpec resolveStrict.	"When we reference a spec, we'll checkout the branch and refresh from remote, as a courtesy"
	repositoryDefinition := theSpec repositoryDefinition.
	repositoryDefinition
		fetch;
		pull: repositoryDefinition remote branch: theSpec revision;
		yourself.
	^ theSpec copy
]

{ #category : 'private' }
RwRowanSample9Test >> _loadSpecSessionCacheKey [
	^ self class _loadSpecSessionCacheKey
]

{ #category : 'private' }
RwRowanSample9Test >> _rowanSample9_0000_load_spec [

	| rowanProject fileUrl |
	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	fileUrl := 'file:' , rowanProject repositoryRootPath
		, '/test/specs/RowanSample9_0000.ston'.
	^(RwSpecification fromUrl: fileUrl)
		projectsHome: self _testRowanProjectsSandbox;
		yourself.
]

{ #category : 'private' }
RwRowanSample9Test >> _specListProjectNames [
	^ super _specListProjectNames , {'PrimerSample9'}
]

{ #category : 'tests' }
RwRowanSample9Test >> testIndexCards [
	| loadSpecification indexCards specsDir loadSpecs projectDefinitions |
	loadSpecification := self _rowanSample9_0000_load_spec.
	loadSpecification repositoryRoot ensureDeleteAll.
	loadSpecification revision: 'master'.
	loadSpecification resolveStrict.

	indexCards := (loadSpecification repositoryRoot / 'indexCards') files
		collect: [ :file | 
			file
				readStreamDo: [ :fileStream | 
					| stream |
					stream := ZnBufferedReadStream on: fileStream.	"wrap with buffered stream to bypass https://github.com/GemTalk/FileSystemGs/issues/9"
					STON fromStream: stream ] ].

	indexCards := indexCards sort: [ :a :b | a index <= b index ].

	specsDir := loadSpecification repositoryRoot / 'specs'.
	loadSpecs := indexCards
		collect: [ :indexCard | 
			specsDir / indexCard specName , 'ston'
				readStreamDo: [ :fileStream | 
					| stream |
					stream := ZnBufferedReadStream on: fileStream.	"wrap with buffered stream to bypass https://github.com/GemTalk/FileSystemGs/issues/9"
					(STON fromStream: stream)
						yourself ] ].

	projectDefinitions := loadSpecs
		collect: [ :loadSpec | STON toStringPretty: loadSpec resolve ].
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue_230 [
	"https://github.com/dalehenrich/Rowan/issues/230"

	| loadSpec projectName projectDefinition loadedProjects project testResult |

false ifTrue: [ ^self ].	"not ready to run"

	loadSpec := self _loadSpecNamed: 'spec_0003'.	"primer"
	projectName := loadSpec projectAlias.

	(Rowan image loadedProjectNamed:projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"resolve/load"
	projectDefinition := loadSpec resolve.
	loadedProjects := projectDefinition load.

"validate"
	self assert: loadedProjects size = 1.
	project := loadedProjects at: 1.
	self assert: project audit isEmpty.
	testResult := project testSuite run.
	self deny: testResult hasErrors.
	self deny: testResult hasFailures.

	loadSpec := self _loadSpecNamed: 'spec_0002'.	"boom"

"resolve/load"
	projectDefinition := loadSpec resolve.
	loadedProjects := projectDefinition load.

"validate"
	self assert: loadedProjects size = 1.
	project := loadedProjects at: 1.
	self assert: project audit isEmpty.
	testResult := project testSuite run.
	self deny: testResult hasErrors.
	self deny: testResult hasFailures.
]

{ #category : 'tests' }
RwRowanSample9Test >> testIssue_527 [
	"
		self _clearLoadSpecSessionCache
	"

	| loadSpec symDictName className projectName |
	loadSpec := self _loadSpecNamed: 'spec_0001'.

	(Rowan image loadedProjectNamed: loadSpec projectAlias ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

	projectName := loadSpec projectAlias.
	className := projectName , 'Class1'.

	self
		_issue_527_resolve_load_validate: loadSpec
		className: className
		expectedSymDictName: self _sampleSymbolDictionaryName1.

	symDictName := self _sampleSymbolDictionaryName2.
	loadSpec
		gemstoneSetDefaultSymbolDictNameForUser: Rowan image currentUserId
		to: symDictName.

	self
		_issue_527_resolve_load_validate: loadSpec
		className: className
		expectedSymDictName: self _sampleSymbolDictionaryName2
]

{ #category : 'tests' }
RwRowanSample9Test >> testRowanSample4_basic [

	| loadSpec projectName projectDefinition loadedProjects project testResult |
	loadSpec := self _loadSpecNamed: 'spec_0002'.

	projectName := loadSpec projectAlias.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"resolve/load"
	projectDefinition := loadSpec resolve.
	loadedProjects := projectDefinition load.

"validate"
	self assert: loadedProjects size = 1.
	project := loadedProjects at: 1.
	self assert: project audit isEmpty.
	testResult := project testSuite run.
	self deny: testResult hasErrors.
	self deny: testResult hasFailures.

"unload project"
	project unload
]

{ #category : 'tests' }
RwRowanSample9Test >> testRowanSample4_primer [

	| loadSpec projectName projectDefinition loadedProjects project testResult |
	loadSpec := self _loadSpecNamed: 'spec_0003'.

	projectName := loadSpec projectAlias.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"resolve/load"
	projectDefinition := loadSpec resolve.
	loadedProjects := projectDefinition load.

"validate"
	self assert: loadedProjects size = 1.
	project := loadedProjects at: 1.
	self assert: project audit isEmpty.
	testResult := project testSuite run.
	self deny: testResult hasErrors.
	self deny: testResult hasFailures.

"unload project"
	project unload
]
