"
No class-specific documentation for CypFileUrl, hierarchy is: 
Object
  CypUrl
    CypFileUrl

"
Class {
	#name : 'CypFileUrl',
	#superclass : 'CypUrl',
	#instVars : [
		'host',
		'path',
		'isAbsolute'
	],
	#category : 'Cypress-Core'
}

{ #category : 'comparing' }
CypFileUrl >> = aFileUrl [

	^self class = aFileUrl class & (host = aFileUrl host)
		& (fragment = aFileUrl fragment) & (isAbsolute = aFileUrl isAbsolute)
		& (path = aFileUrl path)
]

{ #category : 'Accessing' }
CypFileUrl >> fileName [
	"Return the last part of the path,
	most often a filename but can also be a directory."

	^path last
]

{ #category : 'private' }
CypFileUrl >> firstPartIsDriveLetter [
	"Return true if the first part of the path is a letter
	followed by a $: like 'C:' "

	| firstPart |
	path isEmpty ifTrue: [^false].
	firstPart := path first.
	^firstPart size = 2
		and: [firstPart first isLetter and: [firstPart last = $:]]
]

{ #category : 'comparing' }
CypFileUrl >> hash [

	^host hash bitXor: path hash
]

{ #category : 'Accessing' }
CypFileUrl >> host [
	^host

]

{ #category : 'Updating' }
CypFileUrl >> host: newValue [
	host := newValue

]

{ #category : 'private' }
CypFileUrl >> initializeFromPathString: aPathString [
	"<aPathString> is a file path as a String.
	We construct a path collection using various heuristics."

	| pathString hasDriveLetter |
	pathString := aPathString.
	pathString isEmpty ifTrue: [pathString := '/'].
	"Copy without empty string preceeding first / or between duplicated /s."
	path := ((pathString subStrings: '/') copyWithout: '')
				collect: [:token | token unescapePercents].

	"A path like 'C:' refers in practice to 'c:/'"
	((pathString endsWith: '/')
		or: [(hasDriveLetter := self firstPartIsDriveLetter) and: [path size = 1]])
			ifTrue: [path add: ''].

	"Decide if we are absolute by checking for leading $/ or
	beginning with drive letter. Smarts for other OSes?"
	isAbsolute := (pathString beginsWith: '/')
				or: [hasDriveLetter ifNil: [self firstPartIsDriveLetter]]
]

{ #category : 'Accessing' }
CypFileUrl >> isAbsolute [
	^isAbsolute

]

{ #category : 'Updating' }
CypFileUrl >> isAbsolute: newValue [
	isAbsolute := newValue

]

{ #category : 'Accessing' }
CypFileUrl >> path [
	^path

]

{ #category : 'Updating' }
CypFileUrl >> path: newValue [
	path := newValue

]

{ #category : 'paths' }
CypFileUrl >> pathForDirectory [
	"Path using local file system's pathname delimiter.
	DOS paths with drive letters should not
	be prepended with a delimiter even though
	they are absolute. Filename is left out."

	| delimiter |
	delimiter := Rowan fileUtilities pathNameDelimiter.
	^String streamContents: 
			[:s |
			(self isAbsolute and: [self firstPartIsDriveLetter not])
				ifTrue: [s nextPutAll: delimiter].
			1 to: self path size - 1
				do: 
					[:ii |
					s
						nextPutAll: (path at: ii);
						nextPutAll: delimiter]]
]

{ #category : 'private' }
CypFileUrl >> privateInitializeFromText: aString [
	"Calculate host and path from a file URL in String format.
	Some malformed formats are allowed and interpreted by guessing."

	| schemeName pathString bare i |
	bare := aString trimSeparators.
	schemeName := Url schemeNameForString: bare.
	(schemeName isNil or: [schemeName ~= self schemeName])
		ifTrue: 
			[host := ''.
			pathString := bare]
		ifFalse: 
			["First remove schemeName and colon"
			bare := bare copyFrom: schemeName size + 2 to: bare size.
			"A proper file URL then has two slashes before host,
			A malformed URL is interpreted as using syntax file:<path>."
			(bare beginsWith: '//')
				ifTrue: 
					[i := bare indexOf: $/ startingAt: 3.
					i = 0
						ifTrue: 
							[host := bare copyFrom: 3 to: bare size.
							pathString := '']
						ifFalse: 
							[host := bare copyFrom: 3 to: i - 1.
							pathString := bare copyFrom: host size + 3 to: bare size]]
				ifFalse: 
					[host := ''.
					pathString := bare]].
	self initializeFromPathString: pathString
]

{ #category : 'private' }
CypFileUrl >> schemeName [

	^self class schemeName
]
