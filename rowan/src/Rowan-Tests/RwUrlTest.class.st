Class {
	#name : 'RwUrlTest',
	#superclass : 'RwAbstractTest',
	#category : 'Rowan-Tests'
}

{ #category : 'as yet unclassified' }
RwUrlTest class >> testParseNoErrors [
  "list of all of the url strings that should at least parse ... other tests should go into specific details of accessing fields"

  {'github://GsDevKit/GsDevKit:master/repository'.
  'gitfiletree://gitlab.com/GsDevKit/GsDevKit:350/repository'.
  'file:$GS_HOME/shared/repos/rowan/sample/'.
  'cypress:$GS_HOME/shared/repos/rowan/sample/'.
  'filetree://$GS_HOME/shared/repos/rowan/sample/'.
  'tonel://$GS_HOME/shared/repos/rowan/sample/'.
  'https://raw.githubusercontent.com/dalehenrich/sample/master/specs/Sample.ston'.
  'cypress:$GS_HOME/shared/repos/rowan/sample/repository/'.
  'smalltalk://github.com/dalehenrich/CypressReferenceImplementation:rowan/tonel'.
  'smalltalk://github.com/dalehenrich/CypressReferenceImplementation:rowan/tonel/'.
  'smalltalk://github.com/dalehenrich/CypressReferenceImplementation:rowan/'.
  'smalltalk://github.com/dalehenrich/CypressReferenceImplementation/'.
  'http://chaos.resnet.gatech.edu:8000/docs/java/index.html?A%20query%20#part'.
  'msw://chaos.resnet.gatech.edu:9000/testbook?top'.
  'telnet:chaos.resnet.gatech.edu'}
    do: [ :urlString | RwUrl fromString: urlString ]
]

{ #category : 'private' }
RwUrlTest >> sampleFileUrlStrings [
	"file: url, pathString pairs"

	"the complicated bit is the env variable in the url"

	^ {
		{ 'file:/home/dhenrich/_home/shared/repos/rowan/sample/' . '/home/dhenrich/_home/shared/repos/rowan/sample/' }.
		{ 'file:/home/dhenrich/_home/shared/repos/rowan/sample/spec.ston' . '/home/dhenrich/_home/shared/repos/rowan/sample/spec.ston' }.
		{ 'file:$GEMSTONE/shared/repos/rowan/sample/spec.ston' . '$GEMSTONE/shared/repos/rowan/sample/spec.ston' }.
	}
]

{ #category : 'private' }
RwUrlTest >> sampleUrlStrings [
  ^ {'github://GsDevKit/GsDevKit:master/repository'.
  'gitfiletree://gitlab.com/GsDevKit/GsDevKit:350/repository'.
  'file:$GS_HOME/shared/repos/rowan/sample/'.
  'cypress:$GS_HOME/shared/repos/rowan/sample/'.
  'filetree://$GS_HOME/shared/repos/rowan/sample/'.
  'tonel://$GS_HOME/shared/repos/rowan/sample/'.
  'https://raw.githubusercontent.com/dalehenrich/sample/master/specs/Sample.ston'.
  'cypress:$GS_HOME/shared/repos/rowan/sample/repository/'.
  'metacello://github.com/dalehenrich/CypressReferenceImplementation:rowan/tonel'.
  'metacello://github.com/dalehenrich/CypressReferenceImplementation:rowan/tonel/'.
  'metacello://github.com/dalehenrich/CypressReferenceImplementation:rowan/'.
  'metacello://github.com/dalehenrich/CypressReferenceImplementation/'.
  'http://chaos.resnet.gatech.edu:8000/docs/java/index.html?A%20query%20#part'.
  'msw://chaos.resnet.gatech.edu:9000/testbook?top'.
  'telnet:chaos.resnet.gatech.edu'}
]

{ #category : 'tests' }
RwUrlTest >> testAsRwRepository [

	| repos repoRoot testRepoPath loadedProject |
	loadedProject := Rowan image _projectForNonTestProject: 'Rowan'.
	repoRoot :=  loadedProject repositoryRootPath.
	testRepoPath := self _testRowanProjectsSandbox pathString.
	repos := {('cypress:' , testRepoPath , '/cypress/').
	('filetree://' , testRepoPath , '/filetree/').
	('tonel:' , testRepoPath , '/tonel/')}
		collect: [ :urlString | urlString -> urlString asRwRepository ].
	repos
		do: [ :assoc | 
			| repo x y |
			repo := assoc value.
			self assert: (x := repo url) = (y := assoc key) ]
]

{ #category : 'tests' }
RwUrlTest >> testAsRwUrl [
  self sampleUrlStrings
    do: [ :urlString | 
      | url |
      RwUrl fromString: urlString.
      url := urlString asRwUrl.
      self assert: url printString = urlString ]
]

{ #category : 'tests' }
RwUrlTest >> testAsRwUrlForFileUrls [

	self sampleFileUrlStrings
		do: [ :ar | 
			| url urlString fileString pathString dirString x |
			urlString := ar at: 1.
			fileString := ar at: 2.
			RwUrl fromString: urlString.
			url := urlString asRwUrl.
			pathString := url pathString.
			dirString := url pathDirString.
			self assert: (pathString = fileString).
			self assert: url printString = urlString.
			pathString last = $/
				ifTrue: [  self assert: dirString = pathString ]
				ifFalse: [ 
					"use asFileReference to expand env vars in path"
					self assert: (dirString asFileReference pathString) = (x :=  pathString asFileReference parent pathString) ] ]
]
