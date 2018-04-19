"
All Cypress classes are private to GemStone and are likely to be removed in a future release.

A URL which has a hierarchical encoding.  For instance, http and ftp URLs are hierarchical.
"
Class {
	#name : 'CypressHierarchicalUrl',
	#superclass : 'CypressUrl',
	#instVars : [
		'schemeName',
		'authority',
		'path',
		'query',
		'port',
		'username',
		'password'
	],
	#category : 'Cypress-Network-Url'
}

{ #category : 'instance creation' }
CypressHierarchicalUrl class >> schemeName: schemeName  authority: authority  path: path  query: query [
	^self new schemeName: schemeName  authority: authority  path: path  query: query

]

{ #category : 'access' }
CypressHierarchicalUrl >> authority [
	^authority

]

{ #category : 'access' }
CypressHierarchicalUrl >> fileName [
	"Return the last part of the path,
	most often a filename but does not need to be."

	^self path last

]

{ #category : 'printing' }
CypressHierarchicalUrl >> fullPath [

	| ans |
	ans := WriteStreamPortable on: String new.
	path do: 
			[:pathElem |
			ans nextPut: $/.
			self writeWithHttpEscapes: pathElem on: ans].
	self query isNil
		ifFalse: 
			[ans nextPut: $?.
			ans nextPutAll: self query].
	self fragment isNil
		ifFalse: 
			[ans nextPut: $#.
			self writeWithHttpEscapes: self fragment on: ans].
	^ans contents

]

{ #category : 'downloading' }
CypressHierarchicalUrl >> hasContents [
	"most of these do...."
	^true

]

{ #category : 'access' }
CypressHierarchicalUrl >> isAbsolute [
	
	path size > 0 ifFalse: [^ false].
	(path at: 1) size > 0 ifFalse: [^ false].
	^ ((path at: 1) at: 1) ~~ $.

]

{ #category : 'access' }
CypressHierarchicalUrl >> password [
	"http://user:pword@foo.com' asUrl password"
	^password

]

{ #category : 'access' }
CypressHierarchicalUrl >> path [
	"return a collection of the decoded path elements, as strings"
	^path

]

{ #category : 'access' }
CypressHierarchicalUrl >> path: aCollection [
	"Set the collection of path elements."

	path := aCollection

]

{ #category : 'access' }
CypressHierarchicalUrl >> port [
	^port

]

{ #category : 'copying' }
CypressHierarchicalUrl >> postCopy [
	"Be sure not to share the path with the copy"

	super postCopy.
	path := path copy

]

{ #category : 'printing' }
CypressHierarchicalUrl >> printOn: aStream [

	aStream nextPutAll: self schemeName.
	aStream nextPutAll: '://'.
	self username
		ifNotNil: 
			[self writeWithHttpEscapes: self username on: aStream.
			self password
				ifNotNil: 
					[aStream nextPutAll: ':'.
					self writeWithHttpEscapes: self password on: aStream].
			aStream nextPutAll: '@'].
	aStream nextPutAll: self authority.
	port
		ifNotNil: 
			[aStream
				nextPut: $:;
				nextPutAll: port printString].
	path do: 
			[:pathElem |
			aStream nextPut: $/.
			self writeWithHttpEscapes: pathElem on: aStream].
	self query isNil
		ifFalse: 
			[aStream nextPut: $?.
			aStream nextPutAll: self query].
	self fragment isNil
		ifFalse: 
			[aStream nextPut: $#.
			self writeWithHttpEscapes: self fragment on: aStream]

]

{ #category : 'parsing' }
CypressHierarchicalUrl >> privateInitializeFromText: aString [

	| remainder ind specifiedSchemeName |
	remainder := aString.
	schemeName
		ifNil: 
			[specifiedSchemeName := CypressUrl schemeNameForString: remainder.
			specifiedSchemeName
				ifNotNil: 
					[schemeName := specifiedSchemeName.
					remainder := remainder copyFrom: schemeName size + 2 to: remainder size].
			schemeName
				ifNil: 
					["assume HTTP"
					schemeName := 'http']].

	"remove leading // if it's there"
	(remainder indexOfSubCollection: '//' startingAt: 1 ifAbsent: [ 0 ]) = 1
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
	ind := authority indexOf: $@.
	ind > 0
		ifTrue: 
			[username := authority copyFrom: 1 to: ind - 1.
			authority := authority copyFrom: ind + 1 to: authority size.
			ind := username indexOf: $:.
			ind > 0
				ifTrue: 
					[password := (self
								decodeHttpEscapesOf: (username copyFrom: ind + 1 to: username size))
									asByteArray decodeFromUTF8
								asString.
					username := username copyFrom: 1 to: ind - 1]
				ifFalse: [password := nil].
			username := (self decodeHttpEscapesOf: username) asByteArray
						decodeFromUTF8 asString].

	"Extract the port"
	(authority includes: $:)
		ifTrue: 
			[| lastColonIndex portString |
			lastColonIndex := authority findLast: [:c | c = $:].
			portString := authority copyFrom: lastColonIndex + 1 to: authority size.
			(portString allSatisfy: [:each | each isDigit])
				ifTrue: 
					[port := Integer fromString: portString.
					port > 65535 ifTrue: [self error: 'Invalid port number'].
					authority := authority copyFrom: 1 to: lastColonIndex - 1]
				ifFalse: [self error: 'Invalid port number']].

	"get the path"
	path := self privateParsePath: remainder relativeTo: #()

]

{ #category : 'parsing' }
CypressHierarchicalUrl >> privateInitializeFromText: aString relativeTo: aUrl [

	| remainder ind basePath |
	remainder := aString.
	"set the scheme"
	schemeName := aUrl schemeName.

	"a leading // means the authority is specified, meaning it is absolute"
	(remainder indexOfSubCollection: '//' startingAt: 1 ifAbsent: [ 0 ]) = 1
		ifTrue: [^self privateInitializeFromText: aString].

	"otherwise, use the same authority"
	authority := aUrl authority.
	port := aUrl port.
	username := aUrl username.
	password := aUrl password.

	"get the query"
	ind := remainder indexOf: $?.
	ind > 0
		ifTrue: 
			[query := remainder copyFrom: ind + 1 to: remainder size.
			remainder := remainder copyFrom: 1 to: ind - 1].

	"get the path"
	(remainder indexOfSubCollection: '/' startingAt: 1 ifAbsent: [ 0 ]) = 1
		ifTrue: [basePath := #()]
		ifFalse: [basePath := aUrl path].
	path := self privateParsePath: remainder relativeTo: basePath

]

{ #category : 'parsing' }
CypressHierarchicalUrl >> privateParsePath: remainder relativeTo: basePath [

	| nextTok s parsedPath |
	s := remainder readStream.
	parsedPath := OrderedCollection new.
	parsedPath addAll: basePath.
	parsedPath isEmpty ifFalse: [parsedPath removeLast].
	
	[s peek = $/ ifTrue: [s next].
	nextTok := WriteStreamPortable on: String new.
	[s atEnd or: [s peek = $/]] whileFalse: [nextTok nextPut: s next].
	nextTok := self decodeHttpEscapesOf: nextTok contents.
	nextTok = '..'
		ifTrue: [parsedPath size > 0 ifTrue: [parsedPath removeLast]]
		ifFalse: [nextTok ~= '.' ifTrue: [parsedPath add: nextTok]].
	s atEnd]
			whileFalse.
	parsedPath isEmpty ifTrue: [parsedPath add: ''].
	^parsedPath

]

{ #category : 'access' }
CypressHierarchicalUrl >> query [
	"return the query, the part after any ?.  Any %XY's have already been decoded.  If there wasno query part, nil is returned (it is possible to also have an empty query"
	^query 

]

{ #category : 'classification' }
CypressHierarchicalUrl >> scheme [
	^ self schemeName.

]

{ #category : 'access' }
CypressHierarchicalUrl >> schemeName [
	^schemeName

]

{ #category : 'private' }
CypressHierarchicalUrl >> schemeName: schemeName0  authority: authority0  path: path0  query: query0 [
	"initialize a new instance"
	schemeName := schemeName0.
	authority := authority0.
	path := path0.
	query := query0.

]

{ #category : 'access' }
CypressHierarchicalUrl >> username [
	"http://user:pword@foo.com' asUrl username"
	^username

]
