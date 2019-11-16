Class {
	#name : 'RwRowanSample9Test',
	#superclass : 'RwAbstractV2Test',
	#category : 'Rowan-TestsV2'
}

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
	^ {}
]

{ #category : 'tests' }
RwRowanSample9Test >> testIndexCards [
	| loadSpecification indexCards specsDir loadSpecs projectDefinitions |
	loadSpecification := self _rowanSample9_0000_load_spec.
	loadSpecification repositoryRoot ensureDeleteAll.
	loadSpecification revision: 'master'.
	loadSpecification resolve.

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
						repositoryResolutionPolicy: #'strict';
						yourself ] ].

	projectDefinitions := loadSpecs
		collect: [ :loadSpec | STON toStringPretty: loadSpec resolve ].
]
