Class {
	#name : 'RwRowanSample10Test',
	#superclass : 'RwAbstractRowanSampleTest',
	#category : 'Rowan-TestsV2'
}

{ #category : 'private' }
RwRowanSample10Test class >> _gitPullSessionCacheKey [
	^ #'RowanSample10GitBranchDict'
]

{ #category : 'private' }
RwRowanSample10Test class >> _loadSpecSessionCacheKey [
	^ #'RowanSample10LoadSpecsDict'
]

{ #category : 'private' }
RwRowanSample10Test class >> _rowanSample_0000_load_spec [
	| rowanProject fileUrl |
	rowanProject := Rowan image _projectForNonTestProject: 'Rowan'.
	fileUrl := 'file:' , rowanProject repositoryRootPath
		, '/test/specs/RowanSample10_0000.ston'.
	^ (RwSpecification fromUrl: fileUrl)
		projectsHome: self _testRowanProjectsSandbox;
		yourself
]

{ #category : 'private' }
RwRowanSample10Test >> _rowanSample10_0000_load_spec [
	^ self class _rowanSample_0000_load_spec
]

{ #category : 'tests' }
RwRowanSample10Test >> testIndexCards [
	| projectSpecification indexCards specsDir projectSpecs resolvedProjects |
	projectSpecification := self _rowanSample10_0000_load_spec.
	projectSpecification repositoryRoot ensureDeleteAll.
	projectSpecification revision: 'master'.
	self class _adjustSpecForIssue805: projectSpecification.
	projectSpecification resolveStrict.

	indexCards := (projectSpecification repositoryRoot / 'indexCards') files
		collect: [ :file | 
			file
				readStreamDo: [ :fileStream | 
					| stream |
					stream := ZnBufferedReadStream on: fileStream.	"wrap with buffered stream to bypass https://github.com/GemTalk/FileSystemGs/issues/9"
					STON fromStream: stream ] ].

	indexCards := indexCards sort: [ :a :b | a index <= b index ].

	specsDir := projectSpecification repositoryRoot / 'specs'.
	projectSpecs := indexCards
		collect: [ :indexCard | 
			specsDir / indexCard specName , 'ston'
				readStreamDo: [ :fileStream | 
					| stream |
					stream := ZnBufferedReadStream on: fileStream.	"wrap with buffered stream to bypass https://github.com/GemTalk/FileSystemGs/issues/9"
					STON fromStream: stream ] ].

	resolvedProjects := projectSpecs
		collect: [ :projectSpec | 
			projectSpec projectsHome: self _testRowanProjectsSandbox ]
]

{ #category : 'tests' }
RwRowanSample10Test >> testSpec_0001 [
	"spec_0001 should load cleanly"


	| loadSpec projectName resolvedProject loadedProjects |
	loadSpec := self _loadSpecNamed: 'spec_0001'.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"resolve project"
	resolvedProject := loadSpec resolve.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects
]
