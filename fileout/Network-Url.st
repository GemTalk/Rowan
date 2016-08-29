" Package: Network-Url"!


" Class Declarations "!

(Error
	subclass: 'InvalidDirectoryError'
	instVarNames: #( pathName )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Network-Url';
		comment: ''!

(Object
	subclass: 'Authorizer'
	instVarNames: #( users realm )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Network-Url';
		comment: 'The Authorizer does user authorization checking. Each instance of authorizer keeps track of the realm that it is authorizing for, and the table of authorized users. An authorizer can be asked to return the user name/symbol associated with a userID (which concatenates the username and password from the HTTP request) with the user: method.
'!

(Object
	subclass: 'Url'
	instVarNames: #( fragment )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Network-Url';
		comment: 'A Uniform Resource Locator.  It specifies the location of a document on the Internet.  The base class is abstract; child classes break different types of URLs down in ways appropriate for that type.'!

(Url
	subclass: 'GenericUrl'
	instVarNames: #( schemeName locator )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Network-Url';
		comment: 'a URL type that can''t be broken down in any systematic way.  For example, mailto: and telnet: URLs.  The part after the scheme name is stored available via the #locator message.'!

(GenericUrl
	subclass: 'MailtoUrl'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Network-Url';
		comment: 'a URL specifying a mailing address; activating it triggers a mail-sender to start up, if one is present.'!

(GenericUrl
	subclass: 'BrowserUrl'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Network-Url';
		comment: 'URLs that instruct a browser to do something.'!

(Url
	subclass: 'HierarchicalUrl'
	instVarNames: #( schemeName authority path query port username password )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Network-Url';
		comment: 'A URL which has a hierarchical encoding.  For instance, http and ftp URLs are hierarchical.'!

(HierarchicalUrl
	subclass: 'FtpUrl'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Network-Url';
		comment: ''!

(HierarchicalUrl
	subclass: 'HttpUrl'
	instVarNames: #( realm )
	classVars: #( Passwords )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Network-Url';
		comment: 'A URL that can be accessed via the Hypertext Transfer Protocol (HTTP), ie, a standard Web URL

realm = the name of the security realm that has been discovered for this URL.   Look it up in Passwords.

Passwords = a Dictionary of (realm -> encoded user&password)


TODO: use the username and password, if specified
'!

(HttpUrl
	subclass: 'HttpsUrl'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Network-Url';
		comment: ''!

(Url
	subclass: 'FileUrl'
	instVarNames: #( host path isAbsolute )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'Network-Url';
		comment: 'This class models a file URL according to (somewhat) RFC1738, see http://www.w3.org/Addressing/rfc1738.txt

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

   As a special case, <host> can be the string "localhost" or the empty
   string; this is interpreted as `the machine from which the URL is
   being interpreted''.

   The file URL scheme is unusual in that it does not specify an
   Internet protocol or access method for such files; as such, its
   utility in network protocols between hosts is limited.

From the above we can conclude that the RFC says that the <path> part never starts or ends with a slash and is always absolute. If the last name can be a directory instead of a file is not specified clearly.

The path is stored as a SequenceableCollection of path parts.

Notes regarding non RFC features in this class:

- If the last path part is the empty string, then the FileUrl is referring to a directory. This is also shown with a trailing slash when converted to a String.

- The FileUrl has an attribute isAbsolute which signals if the path should be considered absolute or relative to the current directory. This distinction is not visible in the String representation of FileUrl, since the RFC does not have that.

- Fragment is supported (kept for historical reasons)

'!

" Class Implementation for InvalidDirectoryError"!

" ------------------- Class methods for InvalidDirectoryError"!

!InvalidDirectoryError class methodsFor: 'exceptioninstantiator'!
pathName: badPathName
	^self new pathName: badPathName! !
" ------------------- Instance methods for InvalidDirectoryError"!

!InvalidDirectoryError methodsFor: 'exceptiondescription'!
defaultAction

	 ^#()! !
!InvalidDirectoryError methodsFor: 'accessing'!
pathName
	^pathName! !
!InvalidDirectoryError methodsFor: 'accessing'!
pathName: badPathName
	pathName := badPathName! !
" Class Implementation for Authorizer"!

" ------------------- Class methods for Authorizer"!

!Authorizer class methodsFor: 'as yet unclassified'!
unauthorizedFor: realm
	^'HTTP/1.0 401 Unauthorized', self crlf, 'WWW-Authenticate: Basic realm="Pharo/',realm,'"',
	String crlfcrlf, '<html><title>Unauthorized</title><body><h2>Unauthorized for ',realm, '</h2></body></html>'

! !
" ------------------- Instance methods for Authorizer"!

!Authorizer methodsFor: 'authentication'!
encode: nameString password: pwdString
	"Encode per RFC1421 of the username:password combination."

	| clear code clearSize idx map |
	clear := (nameString, ':', pwdString) asByteArray.
	clearSize := clear size.
	[ clear size \\ 3 ~= 0 ] whileTrue: [ clear := clear, #(0) ].
	idx := 1.
	map := 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'.
	code := WriteStream on: String new.
	[ idx < clear size ] whileTrue: [ code 
		nextPut: (map at: (clear at: idx) // 4 + 1);
		nextPut: (map at: (clear at: idx) \\ 4 * 16 + ((clear at: idx + 1) // 16) + 1);
   		nextPut: (map at: (clear at: idx + 1) \\ 16 * 4 + ((clear at: idx + 2) // 64) + 1);
   		nextPut: (map at: (clear at: idx + 2) \\ 64 + 1).
		idx := idx + 3 ].
	code := code contents.
	idx := code size.
	clear size - clearSize timesRepeat: [ code at: idx put: $=. idx := idx - 1].
	^code! !
!Authorizer methodsFor: 'authentication'!
mapFrom: aKey to: aPerson
	"Establish a mapping from a RFC 1421 key to a user."

	users isNil ifTrue: [ users := Dictionary new ].
	aPerson
	 isNil ifTrue: [ users removeKey: aKey ]
	 ifFalse: [
		users removeKey: (users keyAtValue: aPerson ifAbsent: []) ifAbsent: [].
		users at: aKey put: aPerson ]
! !
!Authorizer methodsFor: 'authentication'!
mapName: nameString password: pwdString to: aPerson
	"Insert/remove the encoding per RFC1421 of the username:password combination into/from the UserMap.  DO NOT call this directly, use mapName:password:to: in your ServerAction class.  Only it knows how to record the change on the disk!!"

	self mapFrom: (self encode: nameString password: pwdString) to: aPerson
! !
!Authorizer methodsFor: 'realms'!
realm
	^realm! !
!Authorizer methodsFor: 'realms'!
realm: aString
	realm := aString
! !
!Authorizer methodsFor: 'authentication'!
user: userId
	"Return the requesting user."
	^users at: userId ifAbsent: [ self error: (self class unauthorizedFor: realm) ]! !
" Class Implementation for Url"!

" ------------------- Class methods for Url"!

!Url class methodsFor: 'instance creation'!
absoluteFromFileNameOrUrlString: aString
	"Return a URL from and handle Strings without schemes
	as local relative FileUrls instead of defaulting to a HttpUrl
	as absoluteFromText: does."

	^(Url schemeNameForString: aString)
		ifNil: [aString asUrlRelativeTo: FileUrl workingDirectory]
		ifNotNil: [Url absoluteFromText: aString]! !
!Url class methodsFor: 'instance creation'!
absoluteFromText: aString
	"Return a URL from a string and handle
	a String without a scheme as a HttpUrl."

	"Url absoluteFromText: 'http://chaos.resnet.gatech.edu:8000/docs/java/index.html?A%20query%20#part'" 
	"Url absoluteFromText: 'msw://chaos.resnet.gatech.edu:9000/testbook?top'"
	"Url absoluteFromText: 'telnet:chaos.resnet.gatech.edu'"
	"Url absoluteFromText: 'file:/etc/passwd'"

	| remainder index scheme fragment newUrl |
	"trim surrounding whitespace"
	remainder := aString trimSeparators.	

	"extract the fragment, if any"
	index := remainder indexOf: $#.
	index > 0 ifTrue: [
		fragment := remainder copyFrom: index + 1 to: remainder size.
		remainder := remainder copyFrom: 1 to: index - 1].

	"choose class based on the scheme name, and let that class do the bulk of the parsing"
	scheme := self schemeNameForString: remainder.
	newUrl := (self urlClassForScheme: scheme) new privateInitializeFromText: remainder.
	newUrl privateFragment: fragment.
	^newUrl! !
!Url class methodsFor: 'parsing'!
combine: baseURL withRelative: relURL 
	"Take two URL as string form, combine them and return the corresponding URL in string form"

	^((self absoluteFromText: baseURL) newFromRelativeText: relURL) asString! !
!Url class methodsFor: 'constants'!
schemeName
	"When searching for a class to handle a particular scheme, make sure that Url classes never match by default. This is so that abstract Url classes e.g. HierarchicalUrl can be iterated over, but will not be selected"

	^ nil.! !
!Url class methodsFor: 'parsing'!
schemeNameForString: aString
	"Get the scheme name from a string, or return nil if it's not specified. 
	Used in internal parsing routines - an outsider may as well use asUrl. 
	Return scheme in lowercases."
	
	"Url schemeNameForString: 'http://www.yahoo.com'"
	"Url schemeNameForString: '/etc/passwed'"
	"Url schemeNameForString: '/etc/testing:1.2.3'"

	| index schemeName |
	index := aString indexOf: $: ifAbsent: [^ nil].
	schemeName := aString copyFrom: 1 to: index - 1.
	(schemeName allSatisfy: [:each | each isLetter]) ifFalse: [^ nil].
	^ schemeName asLowercase! !
!Url class methodsFor: 'parsing'!
urlClassForScheme: scheme

	^ self allSubclasses
				detect: [ :urlClass | urlClass schemeName = scheme ]
				ifNone: [ GenericUrl ].! !
" ------------------- Instance methods for Url"!

!Url methodsFor: 'downloading'!
activate
	"spawn an external handler for this URL"
	! !
!Url methodsFor: 'converting'!
asString

	^self printString! !
!Url methodsFor: 'converting'!
asURI
	^self asString asURI! !
!Url methodsFor: 'converting'!
asUrl
	^self! !
!Url methodsFor: 'converting'!
asUrlRelativeTo: aUrl
	^self! !
!Url methodsFor: 'accessing'!
authority
	^''! !
!Url methodsFor: 'converting'!
downloadUrl
	^self asString! !
!Url methodsFor: 'fragment'!
fragment
	^fragment! !
!Url methodsFor: 'downloading'!
hasContents
	"whether this URL can download contents to be displayed; if not, it fundamentally requires an outside application to deal with it.  For example, mailto: and telnet: urls"
	^false! !
!Url methodsFor: 'parsing'!
newFromRelativeText: aString
	"return a URL relative to the current one, given by aString.  For instance, if self is 'http://host/dir/file', and aString is '/dir2/file2', then the return will be a Url for 'http://host/dir2/file2'"

	"if the scheme is the same, or not specified, then use the same class"

	| newSchemeName remainder fragmentStart newFragment newUrl bare |

	bare := aString trimSeparators.
	newSchemeName := Url schemeNameForString: bare.
	(newSchemeName notNil and: [ newSchemeName ~= self schemeName ]) ifTrue: [
		"different scheme -- start from scratch"
		^Url absoluteFromText: aString ].

	remainder := bare.

	"remove the fragment, if any"
	fragmentStart := remainder indexOf: $#.
	fragmentStart > 0 ifTrue: [
		newFragment := remainder copyFrom: fragmentStart+1 to: remainder size. 
		remainder := remainder copyFrom: 1 to: fragmentStart-1].

	"remove the scheme name"
	newSchemeName ifNotNil: [
		remainder := remainder copyFrom: (newSchemeName size + 2) to: remainder size ].

	"create and initialize the new url"
	newUrl := self class new privateInitializeFromText: remainder  relativeTo: self.


	"set the fragment"
	newUrl privateFragment: newFragment.


	^newUrl! !
!Url methodsFor: 'printing'!
printOn: aStream

	^self subclassResponsibility: #printOn:! !
!Url methodsFor: 'fragment'!
privateFragment: aString
	fragment := aString! !
!Url methodsFor: 'parsing'!
privateInitializeFromText: aString

	^self subclassResponsibility: #privateInitializeFromText:! !
!Url methodsFor: 'parsing'!
privateInitializeFromText: aString relativeTo: aUrl
	"initialize from the given string, as a relative URL.  aString will have had the scheme name removed, if it was present to begin with.  If it was, then the scheme name was the same as the receiver's scheme name"

	"by default, just do regular initialization"
	^self privateInitializeFromText: aString! !
!Url methodsFor: 'classification'!
scheme
	"return a string with the scheme of this URL.  For instance, HTTP"

	^self subclassResponsibility: #scheme! !
!Url methodsFor: 'classification'!
schemeName
	"return a lowercase string with the scheme of this URL.  For instance, 'http'"

	^self subclassResponsibility: #schemeName! !
!Url methodsFor: 'fragment'!
withFragment: newFragment
	"return a URL which is the same except that it has a different fragment"
	^self copy privateFragment: newFragment; yourself! !
!Url methodsFor: 'fragment'!
withoutFragment
	"return a URL which is identical to the receiver except that it has no fragment associated with it"
	^self withFragment: nil! !
" Class Implementation for GenericUrl"!

" ------------------- Class methods for GenericUrl"!

!GenericUrl class methodsFor: 'parsing'!
absoluteFromText: aString
	| schemeName locator |
	schemeName := Url schemeNameForString: aString.
	schemeName ifNil: [ ^self schemeName: 'xnoscheme' locator: aString ].
	locator := aString copyFrom: (schemeName size + 2) to: aString size.
	^self schemeName: schemeName locator: locator! !
!GenericUrl class methodsFor: 'instance creation'!
schemeName: schemeName  locator: locator
	^self new schemeName: schemeName  locator: locator! !
" ------------------- Instance methods for GenericUrl"!

!GenericUrl methodsFor: 'access'!
locator
	^locator! !
!GenericUrl methodsFor: 'printing'!
printOn: aStream
	
	self schemeName ifNotNil: [
		aStream nextPutAll: self schemeName; nextPut: $:].
	
	aStream nextPutAll: self locator.

	self fragment ifNotNil: [
		aStream nextPut: $#; nextPutAll: self fragment].! !
!GenericUrl methodsFor: 'parsing'!
privateInitializeFromText: aString
	schemeName := Url schemeNameForString: aString.
	locator :=  schemeName 
		ifNil: [ aString ]
		ifNotNil: [ aString copyFrom: (schemeName size+2) to: aString size ].! !
!GenericUrl methodsFor: 'parsing'!
privateInitializeFromText: aString relativeTo: aUrl
	schemeName := aUrl schemeName.
	locator := aString.! !
!GenericUrl methodsFor: 'classification'!
scheme
	^ self schemeName.! !
!GenericUrl methodsFor: 'access'!
schemeName
	^schemeName! !
!GenericUrl methodsFor: 'private'!
schemeName: schemeName0  locator: locator0
	schemeName := schemeName0.
	locator := locator0.! !
" Class Implementation for MailtoUrl"!

" ------------------- Class methods for MailtoUrl"!

!MailtoUrl class methodsFor: 'constants'!
schemeName

	^ 'mailto'! !
" Class Implementation for BrowserUrl"!

" ------------------- Class methods for BrowserUrl"!

!BrowserUrl class methodsFor: 'constants'!
schemeName

	^ 'browser'! !
" ------------------- Instance methods for BrowserUrl"!

!BrowserUrl methodsFor: 'downloading'!
hasContents
	^true! !
" Class Implementation for HierarchicalUrl"!

" ------------------- Class methods for HierarchicalUrl"!

!HierarchicalUrl class methodsFor: 'instance creation'!
schemeName: schemeName  authority: authority  path: path  query: query
	^self new schemeName: schemeName  authority: authority  path: path  query: query! !
" ------------------- Instance methods for HierarchicalUrl"!

!HierarchicalUrl methodsFor: 'access'!
authority
	^authority! !
!HierarchicalUrl methodsFor: 'access'!
fileName
	"Return the last part of the path,
	most often a filename but does not need to be."

	^self path last! !
!HierarchicalUrl methodsFor: 'printing'!
fullPath
	| ans |
	ans := WriteStream on: String new.
	path do: [ :pathElem |
		ans nextPut: $/.
		ans nextPutAll: pathElem encodeForHTTP. ].
	self query isNil ifFalse: [ 
		ans nextPut: $?.
		ans nextPutAll: self query. ].
	self fragment isNil ifFalse: [
		ans nextPut: $#.
		ans nextPutAll: self fragment encodeForHTTP. ].
	
	^ans contents! !
!HierarchicalUrl methodsFor: 'downloading'!
hasContents
	"most of these do...."
	^true! !
!HierarchicalUrl methodsFor: 'access'!
isAbsolute
	
	path size > 0 ifFalse: [^ false].
	(path at: 1) size > 0 ifFalse: [^ false].
	^ ((path at: 1) at: 1) ~~ $.! !
!HierarchicalUrl methodsFor: 'access'!
password
	"http://user:pword@foo.com' asUrl password"
	^password! !
!HierarchicalUrl methodsFor: 'access'!
path
	"return a collection of the decoded path elements, as strings"
	^path! !
!HierarchicalUrl methodsFor: 'access'!
path: aCollection
	"Set the collection of path elements."

	path := aCollection! !
!HierarchicalUrl methodsFor: 'access'!
port
	^port! !
!HierarchicalUrl methodsFor: 'copying'!
postCopy
	"Be sure not to share the path with the copy"

	super postCopy.
	path := path copy! !
!HierarchicalUrl methodsFor: 'printing'!
printOn: aStream

	aStream nextPutAll: self schemeName.
	aStream nextPutAll: '://'.
	self username ifNotNil: [
		aStream nextPutAll: self username encodeForHTTP.
		self password ifNotNil: [
			aStream nextPutAll: ':'.
			aStream nextPutAll: self password encodeForHTTP].
		aStream nextPutAll: '@' ].
	aStream nextPutAll: self authority.
	port ifNotNil: [aStream nextPut: $:; nextPutAll: port printString].
	path do: [ :pathElem |
		aStream nextPut: $/.
		aStream nextPutAll: pathElem encodeForHTTP. ].
	self query isNil ifFalse: [ 
		aStream nextPut: $?.
		aStream nextPutAll: self query. ].
	self fragment isNil ifFalse: [
		aStream nextPut: $#.
		aStream nextPutAll: self fragment encodeForHTTP. ].! !
!HierarchicalUrl methodsFor: 'parsing'!
privateInitializeFromText: aString 
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
	path := self privateParsePath: remainder relativeTo: #() .! !
!HierarchicalUrl methodsFor: 'parsing'!
privateInitializeFromText: aString relativeTo: aUrl 
	| remainder ind basePath |
	remainder := aString.
	"set the scheme"
	schemeName := aUrl schemeName.

	"a leading // means the authority is specified, meaning it is absolute"
	(remainder beginsWith: '//')
		ifTrue: [^ self privateInitializeFromText: aString].

	"otherwise, use the same authority"
	authority := aUrl authority.
	port := aUrl port.
	username := aUrl username.
	password := aUrl password.

	"get the query"
	ind := remainder indexOf: $?.
	ind > 0
		ifTrue: [query := remainder copyFrom: ind + 1 to: remainder size.
			remainder := remainder copyFrom: 1 to: ind - 1].

	"get the path"
	(remainder beginsWith: '/')
		ifTrue: [ basePath := #() ]
		ifFalse: [ basePath := aUrl path ].
	path := self privateParsePath: remainder  relativeTo: basePath.

! !
!HierarchicalUrl methodsFor: 'parsing'!
privateParsePath: remainder relativeTo: basePath 
	| nextTok s parsedPath |
	s := remainder readStream.
	parsedPath := OrderedCollection new.
	parsedPath addAll: basePath.
	parsedPath isEmpty ifFalse: [ parsedPath removeLast ].
	
	[ s peek = $/ ifTrue: [ s next ].
	nextTok := WriteStream on: String new.
	[ s atEnd or: [ s peek = $/ ] ] whileFalse: [ nextTok nextPut: s next ].
	nextTok := nextTok contents unescapePercents.
	nextTok = '..' 
		ifTrue: [ parsedPath size > 0 ifTrue: [ parsedPath removeLast ] ]
		ifFalse: [ nextTok ~= '.' ifTrue: [ parsedPath add: nextTok ] ].
	s atEnd ] whileFalse.
	parsedPath isEmpty ifTrue: [ parsedPath add: '' ].
	^ parsedPath! !
!HierarchicalUrl methodsFor: 'access'!
query
	"return the query, the part after any ?.  Any %XY's have already been decoded.  If there wasno query part, nil is returned (it is possible to also have an empty query"
	^query ! !
!HierarchicalUrl methodsFor: 'classification'!
scheme
	^ self schemeName.! !
!HierarchicalUrl methodsFor: 'access'!
schemeName
	^schemeName! !
!HierarchicalUrl methodsFor: 'private'!
schemeName: schemeName0  authority: authority0  path: path0  query: query0
	"initialize a new instance"
	schemeName := schemeName0.
	authority := authority0.
	path := path0.
	query := query0.
! !
!HierarchicalUrl methodsFor: 'access'!
username
	"http://user:pword@foo.com' asUrl username"
	^username! !
" Class Implementation for FtpUrl"!

" ------------------- Class methods for FtpUrl"!

!FtpUrl class methodsFor: 'constants'!
schemeName

	^ 'ftp'.! !
" Class Implementation for HttpUrl"!

" ------------------- Class methods for HttpUrl"!

!HttpUrl class methodsFor: 'constants'!
schemeName

	^ 'http'.! !
" Class Implementation for HttpsUrl"!

" ------------------- Class methods for HttpsUrl"!

!HttpsUrl class methodsFor: 'constants'!
schemeName

	^ 'https'.! !
" Class Implementation for FileUrl"!

" ------------------- Class methods for FileUrl"!

!FileUrl class methodsFor: 'instance creation'!
absoluteFromText: aString
	"Method that can be called explicitly to create a FileUrl."

	^self new privateInitializeFromText: aString! !
!FileUrl class methodsFor: 'instance creation'!
host: aHost pathParts: aCollectionOfPathParts isAbsolute: aBoolean
	"Create a FileUrl."

	^self new host: aHost pathParts: aCollectionOfPathParts isAbsolute: aBoolean! !
!FileUrl class methodsFor: 'instance creation'!
pathParts: aCollectionOfPathParts
	"Create a FileUrl."

	^self host: nil pathParts: aCollectionOfPathParts isAbsolute: true! !
!FileUrl class methodsFor: 'instance creation'!
pathParts: aCollectionOfPathParts isAbsolute: aBoolean
	"Create a FileUrl."

	^self host: nil pathParts: aCollectionOfPathParts isAbsolute: aBoolean! !
!FileUrl class methodsFor: 'constants'!
schemeName
	^'file'! !
!FileUrl class methodsFor: 'instance creation'!
workingDirectory

	^self absoluteFromText: CypressFileUtilities current workingDirectory! !
" ------------------- Instance methods for FileUrl"!

!FileUrl methodsFor: 'downloading'!
default
	"Answer a new URL with the receiver's path relative to the current working directory."
	
	self privateInitializeFromText: self pathString relativeTo: self class workingDirectory.
! !
!FileUrl methodsFor: 'accessing'!
fileName
	"Return the last part of the path,
	most often a filename but can also be a directory."

	^self path last! !
!FileUrl methodsFor: 'testing'!
firstPartIsDriveLetter
	"Return true if the first part of the path is a letter
	followed by a $: like 'C:' "
	
	| firstPart |
	path isEmpty ifTrue: [^false].
	firstPart := path first.
	^firstPart size = 2 and: [
		firstPart first isLetter
			and: [firstPart last = $:]]! !
!FileUrl methodsFor: 'downloading'!
hasContents
	^true! !
!FileUrl methodsFor: 'accessing'!
host
	"Return the host name, either 'localhost', '', or a fully qualified domain name."
	
	^host ifNil: ['']! !
!FileUrl methodsFor: 'accessing'!
host: hostName
	"Set the host name, either 'localhost', '', or a fully qualified domain name."
	
	host := hostName! !
!FileUrl methodsFor: 'private-initialization'!
host: aHostString pathParts: aCollection isAbsolute: aBoolean

	host := aHostString.
	path := aCollection.
	isAbsolute := aBoolean! !
!FileUrl methodsFor: 'private-initialization'!
initializeFromPathString: aPathString
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
						or: [hasDriveLetter ifNil: [self firstPartIsDriveLetter]])! !
!FileUrl methodsFor: 'accessing'!
isAbsolute
	"Should the path be considered absolute to
	the filesystem instead of relative to the default directory?"
 
	^isAbsolute! !
!FileUrl methodsFor: 'accessing'!
isAbsolute: aBoolean
	"Set if the path should be considered absolute to
	the filesystem instead of relative to the default directory."

	isAbsolute := aBoolean! !
!FileUrl methodsFor: 'accessing'!
path
	"Return an ordered collection of the path elements."
	
	^path! !
!FileUrl methodsFor: 'accessing'!
path: aCollection
	"Set the collection of path elements."

	path := aCollection! !
!FileUrl methodsFor: 'paths'!
pathDirString
	"Path to directory as url, using slash as delimiter.
	Filename is left out."

	^String streamContents: [ :s |
		isAbsolute ifTrue: [ s nextPut: $/ ].
		1 to: self path size - 1 do: [ :ii |
			s nextPutAll: (path at: ii); nextPut: $/]]! !
!FileUrl methodsFor: 'paths'!
pathForDirectory
	"Path using local file system's pathname delimiter.
	DOS paths with drive letters should not
	be prepended with a delimiter even though
	they are absolute. Filename is left out."

	| delimiter |
	delimiter :=  CypressFileUtilities current pathNameDelimiter.
	^String streamContents: [ :s |
		(self isAbsolute and: [self firstPartIsDriveLetter not])
			ifTrue: [ s nextPutAll: delimiter ].
		1 to: self path size - 1 do: [ :ii |
			s nextPutAll: (path at: ii); nextPutAll: delimiter]]! !
!FileUrl methodsFor: 'private-initialization'!
pathParts: aCollection isAbsolute: aBoolean

	^self host: nil pathParts: aCollection isAbsolute: aBoolean! !
!FileUrl methodsFor: 'paths'!
pathString
	"Path as it appears in a URL with $/ as delimiter."
	
	
	^String streamContents: [ :s | | first |
		"isAbsolute ifTrue:[ s nextPut: $/ ]."
		first := true.
		self path do: [ :p |
			first ifFalse: [ s nextPut: $/ ].
			first := false.
			s nextPutAll: p encodeForHTTP ] ]! !
!FileUrl methodsFor: 'copying'!
postCopy
	"Be sure not to share the path with the copy."

	super postCopy.
	path := path copy! !
!FileUrl methodsFor: 'printing'!
printOn: aStream
	"Return the FileUrl according to RFC1738 plus supporting fragments:
		'file://<host>/<path>#<fragment>'
	Note that <host> being '' is equivalent to 'localhost'.
	Note: The pathString can not start with a leading $/
	to indicate an 'absolute' file path.
	This is not according to RFC1738 where the path should have
	no leading or trailing slashes, and always
	be considered absolute relative to the filesystem."

	aStream nextPutAll: self schemeName, '://'.

	host ifNotNil: [aStream nextPutAll: host].

	aStream
		nextPut: $/;
		nextPutAll: self pathString.

	fragment ifNotNil:
		[aStream
			nextPut: $#;
			nextPutAll: fragment encodeForHTTP].! !
!FileUrl methodsFor: 'private-initialization'!
privateInitializeFromText: aString
	"Calculate host and path from a file URL in String format.
	Some malformed formats are allowed and interpreted by guessing."

	| schemeName pathString bare i |
	bare := aString trimSeparators.
	schemeName := Url schemeNameForString: bare.
	(schemeName isNil or: [schemeName ~= self schemeName])
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
! !
!FileUrl methodsFor: 'private-initialization'!
privateInitializeFromText: pathString relativeTo: aUrl
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

	! !
!FileUrl methodsFor: 'classification'!
scheme
	^self class schemeName! !
!FileUrl methodsFor: 'classification'!
schemeName
	^self class schemeName! !
" Class Extensions "!

" Class Extension for String"!

" ------------------- Instance methods for String"!

!String methodsFor: '*network-url'!
asUrl
	"convert to a Url"
	"'http://www.cc.gatech.edu/' asUrl"
	"msw://chaos.resnet.gatech.edu:9000/' asUrl"
	^Url absoluteFromText: self! !
!String methodsFor: '*network-url'!
asUrlRelativeTo: aUrl
	^aUrl newFromRelativeText: self! !
" Class Extension for FileStream"!

" ------------------- Instance methods for FileStream"!

!FileStream methodsFor: '*Network-Url'!
asUrl
	"Convert my path into a file:// type url - a FileUrl."
	
	^ FileUrl pathParts: (self directory pathSegments copyWith: self localName)! !
" Class initializers "!




" End of Package: Network-Url"!


