"
This class models a file URL according to (somewhat) RFC1738, see http://www.w3.org/Addressing/rfc1738.txt

Here is the relevant part of the RFC:

3.10 FILES

   The file URL scheme is used to designate files accessible on a
   particular host computer. This scheme, unlike most other URL schemes,
   does not designate a resource that is universally accessible over the
   Internet.

   A file URL takes the form:

       file://<host>/<path>

   where <host> is the fully qualified domain name of the system on
   which the <path> is accessible, and <path> is a hierarchical
   directory path of the form <directory>/<directory>/.../<name>.

   For example, a VMS file

     DISK$USER:[MY.NOTES]NOTE123456.TXT

   might become

     <URL:file://vms.host.edu/disk$user/my/notes/note12345.txt>

   As a special case, <host> can be the string ""localhost"" or the empty
   string; this is interpreted as `the machine from which the URL is
   being interpreted'.

   The file URL scheme is unusual in that it does not specify an
   Internet protocol or access method for such files; as such, its
   utility in network protocols between hosts is limited.

From the above we can conclude that the RFC says that the <path> part never starts or ends with a slash and is always absolute. If the last name can be a directory instead of a file is not specified clearly.

The path is stored as a SequenceableCollection of path parts.

Notes regarding non RFC features in this class:

- If the last path part is the empty string, then the FileUrl is referring to a directory. This is also shown with a trailing slash when converted to a String.

- The FileUrl has an attribute isAbsolute which signals if the path should be considered absolute or relative to the current directory. This distinction is not visible in the String representation of FileUrl, since the RFC does not have that.

- Fragment is supported (kept for historical reasons)


"
Class {
	#name : 'RwFileUrl',
	#superclass : 'RwUrl',
	#instVars : [
		'host',
		'path',
		'isAbsolute'
	],
	#category : 'Rowan-Url-Core'
}

{ #category : 'instance creation' }
RwFileUrl class >> fromString: aString [
	"Method that can be called explicitly to create a FileUrl."

	^self new privateInitializeFromText: aString

]

{ #category : 'constants' }
RwFileUrl class >> schemeName [
	^'file'

]

{ #category : 'converting' }
RwFileUrl >> asRwRepository [
  "return a platform appropriate repository for the repository located in the directory referenced by the receiver"

  ^ self createRwRepositoryForPath: self pathForDirectory
]

{ #category : 'accessing' }
RwFileUrl >> fileName [
	"Return the last part of the path,
	most often a filename but can also be a directory."

	^self path last

]

{ #category : 'testing' }
RwFileUrl >> firstPartIsDriveLetter [
	"Return true if the first part of the path is a letter
	followed by a $: like 'C:' "
	
	| firstPart |
	path isEmpty ifTrue: [^false].
	firstPart := path first.
	^firstPart size = 2 and: [
		firstPart first isLetter
			and: [firstPart last = $:]]

]

{ #category : 'accessing' }
RwFileUrl >> host [
	"Return the host name, either 'localhost', '', or a fully qualified domain name."
	
	^host ifNil: ['']

]

{ #category : 'accessing' }
RwFileUrl >> host: hostName [
	"Set the host name, either 'localhost', '', or a fully qualified domain name."
	
	host := hostName

]

{ #category : 'private-initialization' }
RwFileUrl >> host: aHostString pathParts: aCollection isAbsolute: aBoolean [

	host := aHostString.
	path := aCollection.
	isAbsolute := aBoolean

]

{ #category : 'private-initialization' }
RwFileUrl >> initializeFromPathString: aPathString [
	"<aPathString> is a file path as a String.
	We construct a path collection using various heuristics."

	| pathString hasDriveLetter |
	pathString := aPathString.
	pathString isEmpty ifTrue: [pathString := '/'].
	"Copy without empty string preceeding first / or between duplicated /s."
	path := ((pathString subStrings: '/') copyWithout: '') collect: [:token | token unescapePercents].

	"A path like 'C:' refers in practice to 'c:/'"
	((pathString endsWith: '/') or:
		[(hasDriveLetter := self firstPartIsDriveLetter) and: [path size = 1]])
			ifTrue: [path add: ''].

	"Decide if we are absolute by checking for leading $/ or
	beginning with drive letter. Smarts for other OSes?"
	self isAbsolute: ((pathString beginsWith: '/')
						or: [hasDriveLetter ifNil: [self firstPartIsDriveLetter]])

]

{ #category : 'accessing' }
RwFileUrl >> isAbsolute [
	"Should the path be considered absolute to
	the filesystem instead of relative to the default directory?"
 
	^isAbsolute

]

{ #category : 'accessing' }
RwFileUrl >> isAbsolute: aBoolean [
	"Set if the path should be considered absolute to
	the filesystem instead of relative to the default directory."

	isAbsolute := aBoolean

]

{ #category : 'accessing' }
RwFileUrl >> path [
	"Return an ordered collection of the path elements."
	
	^path

]

{ #category : 'accessing' }
RwFileUrl >> path: aCollection [
	"Set the collection of path elements."

	path := aCollection

]

{ #category : 'paths' }
RwFileUrl >> pathDirString [
	"Path to directory as url, using slash as delimiter.
	Filename is left out."

	^String streamContents: [ :s |
		isAbsolute ifTrue: [ s nextPut: $/ ].
		1 to: self path size - 1 do: [ :ii |
			s nextPutAll: (path at: ii); nextPut: $/]]

]

{ #category : 'paths' }
RwFileUrl >> pathForDirectory [
	"Path using local file system's pathname delimiter.
	DOS paths with drive letters should not
	be prepended with a delimiter even though
	they are absolute. Filename is left out."

	| delimiter |
	delimiter :=  '/'.
	^String streamContents: [ :s |
		(self isAbsolute and: [self firstPartIsDriveLetter not])
			ifTrue: [ s nextPutAll: delimiter ].
		1 to: self path size - 1 do: [ :ii |
			s nextPutAll: (path at: ii); nextPutAll: delimiter]]
]

{ #category : 'paths' }
RwFileUrl >> pathForFile [
  ^ self pathString

]

{ #category : 'private-initialization' }
RwFileUrl >> pathParts: aCollection isAbsolute: aBoolean [

	^self host: nil pathParts: aCollection isAbsolute: aBoolean

]

{ #category : 'paths' }
RwFileUrl >> pathString [
	"Path as it appears in a URL with $/ as delimiter."
	
	
	^String streamContents: [ :s | | first |
		self isAbsolute ifTrue:[ s nextPut: $/ ].
		first := true.
		self path do: [ :p |
			first ifFalse: [ s nextPut: $/ ].
			first := false.
			s nextPutAll: p  ] ]
]

{ #category : 'copying' }
RwFileUrl >> postCopy [
	"Be sure not to share the path with the copy."

	super postCopy.
	path := path copy

]

{ #category : 'printing' }
RwFileUrl >> printOn: aStream [
	"Return the FileUrl according to RFC3986
		'file:'['//'<host>]<path>#<fragment>
	Note that <host> being '' is equivalent to 'localhost' and is not printed."

	aStream nextPutAll: self schemeName;
		nextPut: $:.

	"File URLs with hosts (which are fairly useless) cannot be relative."
	host isEmpty ifFalse: [isAbsolute ifFalse: [aStream nextPutAll: '<ErroneousURL>'. ^nil].
						aStream nextPutAll: '//';
						nextPutAll: host].

	aStream
		nextPutAll: self pathString.

	fragment ifNotNil:
		[aStream
			nextPut: $#;
			nextPutAll: fragment ].
]

{ #category : 'private-initialization' }
RwFileUrl >> privateInitializeFromText: aString [
	"Calculate host and path from a file URL in String format.
	Some malformed formats are allowed and interpreted by guessing."

	| schemeName pathString bare i |
	bare := aString trimSeparators.
	schemeName := RwUrl schemeNameForString: bare.
	(schemeName == nil  or: [schemeName ~= self schemeName])
		ifTrue: [
			host := ''.
			pathString := bare]
		ifFalse: [
			"First remove schemeName and colon"
			bare := bare copyFrom: (schemeName size + 2) to: bare size.
			"A proper file URL then has two slashes before host,
			A malformed URL is interpreted as using syntax file:<path>."
			(bare beginsWith: '//')
				ifTrue: [i := bare indexOf: $/ startingAt: 3.
						i=0 ifTrue: [
								host := bare copyFrom: 3 to: bare size.
								pathString := '']
							ifFalse: [
								host := bare copyFrom: 3 to: i-1.
								pathString := bare copyFrom: host size + 3 to: bare size]]
				ifFalse: [host := ''.
						pathString := bare]].
	self initializeFromPathString: pathString

]

{ #category : 'private-initialization' }
RwFileUrl >> privateInitializeFromText: pathString relativeTo: aUrl [
	"<pathString> should be a filesystem path.
	This url is adjusted to be aUrl + the path."

	| newPath |
	self host: aUrl host.
	self initializeFromPathString: pathString.
	self isAbsolute: aUrl isAbsolute.

	newPath := aUrl path copy.
	newPath removeLast.	"empty string that says its a directory"
	path do: [ :token |
		((token ~= '..') and: [token ~= '.']) ifTrue: [ 
			newPath addLast: token unescapePercents ].
		token = '..' ifTrue: [ 
			newPath isEmpty ifFalse: [ 
				newPath last = '..' ifFalse: [ newPath removeLast ] ] ].
		"token = '.' do nothing" ].
	path := newPath

	

]

{ #category : 'classification' }
RwFileUrl >> scheme [
	^self class schemeName

]

{ #category : 'classification' }
RwFileUrl >> schemeName [
	^self class schemeName

]

{ #category : 'accessing' }
RwFileUrl >> segments [

	^ path
]
