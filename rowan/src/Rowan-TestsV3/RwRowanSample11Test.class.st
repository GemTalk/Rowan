Class {
	#name : 'RwRowanSample11Test',
	#superclass : 'RwAbstractRowanSampleTest',
	#category : 'Rowan-TestsV3'
}

{ #category : 'private' }
RwRowanSample11Test class >> _gitPullSessionCacheKey [
	^ #'RowanSample11GitBranchDict'
]

{ #category : 'private' }
RwRowanSample11Test class >> _loadSpecSessionCacheKey [
	^ #'RowanSample11LoadSpecsDict'
]

{ #category : 'private' }
RwRowanSample11Test class >> _rowanSample_0000_load_spec [
	^ (RwSpecification
		fromUrl:
			'https://raw.githubusercontent.com/dalehenrich/RowanSample11/master/specs/spec_0000.ston')
		projectsHome: self _testRowanProjectsSandbox;
		yourself
]

{ #category : 'private' }
RwRowanSample11Test >> _rowanSample10_0000_load_spec [
	^ self class _rowanSample_0000_load_spec
]

{ #category : 'tests' }
RwRowanSample11Test >> testIndexCards [
	| projectSpecification indexCards specsDir projectSpecs resolvedProjects |
	projectSpecification := self _rowanSample10_0000_load_spec.
	projectSpecification repositoryRoot ensureDeleteAll.
	projectSpecification revision: 'master'.
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
			projectSpec projectsHome: self _testRowanProjectsSandbox.
			STON toStringPretty: projectSpec resolveProject ]
]

{ #category : 'tests' }
RwRowanSample11Test >> testSpec_0001 [
	"spec_0001 should load cleanly"


	| loadSpec projectName resolvedProject loadedProjects |
	loadSpec := self _loadSpecNamed: 'spec_0001'.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"resolve project"
	resolvedProject := loadSpec resolveProject.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects
]

{ #category : 'tests' }
RwRowanSample11Test >> testSpec_0002 [
	"spec_0002 should load cleanly"


	| loadSpec projectName resolvedProject loadedProjects |
	loadSpec := self _loadSpecNamed: 'spec_0002'.

	projectName := loadSpec projectName.

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ].

"resolve project"
	resolvedProject := loadSpec resolveProject.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	self _standard_validate: resolvedProject loadedProjects: loadedProjects
]

{ #category : 'tests' }
RwRowanSample11Test >> testSpec_0004 [
	"spec_0004 should load cleanly along with other recursive required projects"

	"https://github.com/GemTalk/Rowan/issues/701"

	| loadSpec projectName projectNames resolvedProject loadedProjects requiredProjectName1 requiredProjectName2 requiredProjectName3 requiredProjectNames |
	loadSpec := self _loadSpecNamed: 'spec_0004'.

	projectName := loadSpec projectName.
	requiredProjectName1 := 'RowanSample9'.
	requiredProjectName2 := 'RowanSample10'.
	requiredProjectName3 := 'RowanSample12'.
	requiredProjectNames :=  { requiredProjectName1 . requiredProjectName2 . requiredProjectName3}.
	projectNames := {projectName }, requiredProjectNames.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ].
	requiredProjectNames do: [:pn |
		(self _testRowanProjectsSandbox / pn ) ensureDeleteAll ].

"resolve project"
	resolvedProject := loadSpec resolveProject.

"load project"
	loadedProjects := resolvedProject load.

"validate"
	self
		_standard_validate: resolvedProject
		loadedProjects: loadedProjects
		expectedProjectNames: projectNames.
	self deny: (loadedProjects projectNamed: projectName) testSuite tests isEmpty.
	self deny: (loadedProjects projectNamed: requiredProjectName1) testSuite tests isEmpty.
	self deny: (loadedProjects projectNamed: requiredProjectName2) testSuite tests isEmpty.
	self deny: (loadedProjects projectNamed: requiredProjectName3) testSuite tests isEmpty.

	projectNames do: [:pn | 
		(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
			ifNotNil: [ :proj | Rowan image _removeLoadedProject: proj ] ]
]
