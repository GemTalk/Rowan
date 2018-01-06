Class {
	#name : 'RwUrlTest',
	#superclass : 'TestCase',
	#category : 'Rowan-Tode'
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
  | repos |
  repos := {'cypress:$GS_HOME/shared/repos/rowan/CypressReferenceImplementation/cypress/'.
  'filetree://$GS_HOME/shared/repos/rowan/CypressReferenceImplementation/filetree/'.
  'tonel://$GS_HOME/shared/repos/rowan/CypressReferenceImplementation/tonel/'}
    collect: [ :urlString | urlString asRwRepository ]
]

{ #category : 'tests' }
RwUrlTest >> testParseNoErrors [
  self sampleUrlStrings
    do: [ :urlString | 
      RwUrl fromString: urlString.
      urlString asRwUrl ]
]
