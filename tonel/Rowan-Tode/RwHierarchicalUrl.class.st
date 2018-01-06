"
A URL which has a hierarchical encoding.  For instance, http and ftp URLs are hierarchical.
"
Class {
	#name : 'RwHierarchicalUrl',
	#superclass : 'RwUrl',
	#instVars : [
		'schemeName',
		'authority',
		'path',
		'query',
		'port',
		'username',
		'password'
	],
	#category : 'Rowan-Tode'
}

{ #category : 'instance creation' }
RwHierarchicalUrl class >> schemeName: schemeName  authority: authority  path: path  query: query [
	^self new schemeName: schemeName  authority: authority  path: path  query: query

]

{ #category : 'access' }
RwHierarchicalUrl >> authority [
	^authority

]

{ #category : 'printing' }
RwHierarchicalUrl >> fullPath [
  | ans |
  ans := WriteStreamPortable on: String new.
  path
    do: [ :pathElem | 
      ans nextPut: $/.
      ans nextPutAll: pathElem ].
  self query == nil
    ifFalse: [ 
      ans nextPut: $?.
      ans nextPutAll: self query ].
  self fragment == nil
    ifFalse: [ 
      ans nextPut: $#.
      ans nextPutAll: self fragment ].
  ^ ans contents
]

{ #category : 'access' }
RwHierarchicalUrl >> isAbsolute [
	
	path size > 0 ifFalse: [^ false].
	(path at: 1) size > 0 ifFalse: [^ false].
	^ ((path at: 1) at: 1) ~~ $.

]

{ #category : 'access' }
RwHierarchicalUrl >> password [
	"http://user:pword@foo.com' asUrl password"
	^password

]

{ #category : 'access' }
RwHierarchicalUrl >> path [
	"return a collection of the decoded path elements, as strings"
	^path

]

{ #category : 'access' }
RwHierarchicalUrl >> path: aCollection [
	"Set the collection of path elements."

	path := aCollection

]

{ #category : 'access' }
RwHierarchicalUrl >> port [
	^port

]

{ #category : 'copying' }
RwHierarchicalUrl >> postCopy [
	"Be sure not to share the path with the copy"

	super postCopy.
	path := path copy

]

{ #category : 'printing' }
RwHierarchicalUrl >> printOn: aStream [
  aStream nextPutAll: self schemeName.
  aStream nextPutAll: '://'.
  self username
    ifNotNil: [ 
      aStream nextPutAll: self username.
      self password
        ifNotNil: [ 
          aStream nextPutAll: ':'.
          aStream nextPutAll: self password ].
      aStream nextPutAll: '@' ].
  aStream nextPutAll: self authority.
  port
    ifNotNil: [ 
      aStream
        nextPut: $:;
        nextPutAll: port printString ].
  path
    do: [ :pathElem | 
      aStream nextPut: $/.
      aStream nextPutAll: pathElem ].
  self query == nil
    ifFalse: [ 
      aStream nextPut: $?.
      aStream nextPutAll: self query ].
  self fragment == nil
    ifFalse: [ 
      aStream nextPut: $#.
      aStream nextPutAll: self fragment ]
]

{ #category : 'parsing' }
RwHierarchicalUrl >> privateInitializeFromText: aString [ 
	| remainder ind specifiedSchemeName |
	remainder := aString.
	schemeName ifNil: 
			[specifiedSchemeName := Url schemeNameForString: remainder.
			specifiedSchemeName ifNotNil: 
					[schemeName := specifiedSchemeName.
					remainder := remainder copyFrom: schemeName size + 2 to: remainder size].
			schemeName ifNil: 
					["assume HTTP"
					schemeName := 'http']].

	"remove leading // if it's there"
	(remainder beginsWith: '//') 
		ifTrue: [remainder := remainder copyFrom: 3 to: remainder size].

	"get the query"
	ind := remainder indexOf: $?.
	ind > 0 
		ifTrue: 
			[query := remainder copyFrom: ind + 1 to: remainder size.
			remainder := remainder copyFrom: 1 to: ind - 1].

	"get the authority"
	ind := remainder indexOf: $/.
	ind > 0 
		ifTrue: 
			[ind = 1 
				ifTrue: [authority := '']
				ifFalse: 
					[authority := remainder copyFrom: 1 to: ind - 1.
					remainder := remainder copyFrom: ind + 1 to: remainder size]]
		ifFalse: 
			[authority := remainder.
			remainder := ''].

	"extract the username+password"
	(authority includes: $@) 
		ifTrue: 
			[username := authority copyUpTo: $@.
			authority := authority copyFrom: (authority indexOf: $@) + 1
						to: authority size.
			(username includes: $:) 
				ifTrue: 
					[password := (username copyFrom: (username indexOf: $:) + 1 to: username size) unescapePercents asByteArray decodeFromUTF8 asString.
					username := (username copyUpTo: $:) unescapePercents asByteArray decodeFromUTF8 asString]
				ifFalse: 
					[password := nil.
					username := username unescapePercents asByteArray decodeFromUTF8 asString]].

	"Extract the port"
	(authority includes: $:) 
		ifTrue: 
			[| lastColonIndex portString |
			lastColonIndex := authority findLast: [:c | c = $:].
			portString := authority copyFrom: lastColonIndex + 1 to: authority size.
			(portString allSatisfy: [:each | each isDigit])
				ifTrue: 
					[port := Integer fromString: portString.
					(port > 65535) ifTrue: [self error: 'Invalid port number'].
					 authority := authority copyFrom: 1 to: lastColonIndex - 1]
				ifFalse:[self error: 'Invalid port number']].

	"get the path"
	path := self privateParsePath: remainder relativeTo: #() .

]

{ #category : 'parsing' }
RwHierarchicalUrl >> privateParsePath: remainder relativeTo: basePath [ 
	| nextTok s parsedPath |
	s := remainder readStream.
	parsedPath := OrderedCollection new.
	parsedPath addAll: basePath.
	parsedPath isEmpty ifFalse: [ parsedPath removeLast ].
	
	[ s peek = $/ ifTrue: [ s next ].
	nextTok := WriteStreamPortable on: String new.
	[ s atEnd or: [ s peek = $/ ] ] whileFalse: [ nextTok nextPut: s next ].
	nextTok := nextTok contents unescapePercents.
	nextTok = '..' 
		ifTrue: [ parsedPath size > 0 ifTrue: [ parsedPath removeLast ] ]
		ifFalse: [ nextTok ~= '.' ifTrue: [ parsedPath add: nextTok ] ].
	s atEnd ] whileFalse.
	parsedPath isEmpty ifTrue: [ parsedPath add: '' ].
	^ parsedPath

]

{ #category : 'access' }
RwHierarchicalUrl >> query [
	"return the query, the part after any ?.  Any %XY's have already been decoded.  If there wasno query part, nil is returned (it is possible to also have an empty query"
	^query 

]

{ #category : 'classification' }
RwHierarchicalUrl >> scheme [
	^ self schemeName.

]

{ #category : 'access' }
RwHierarchicalUrl >> schemeName [
	^schemeName

]

{ #category : 'private' }
RwHierarchicalUrl >> schemeName: schemeName0  authority: authority0  path: path0  query: query0 [
	"initialize a new instance"
	schemeName := schemeName0.
	authority := authority0.
	path := path0.
	query := query0.

]

{ #category : 'access' }
RwHierarchicalUrl >> username [
	"http://user:pword@foo.com' asUrl username"
	^username

]
