" Package: NetworkTests"!


" Class Declarations "!

(TestCase
	subclass: 'NetworkTestCase'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'NetworkTests';
		comment: ''!

(NetworkTestCase
	subclass: 'HTTPEncodingTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'NetworkTests-Protocols';
		comment: ''!

(NetworkTestCase
	subclass: 'HttpUrlTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'NetworkTests-Url';
		comment: ''!

(NetworkTestCase
	subclass: 'HierarchicalUrlTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'NetworkTests-Url';
		comment: ''!

(NetworkTestCase
	subclass: 'GenericUrlTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'NetworkTests-Url';
		comment: ''!

(NetworkTestCase
	subclass: 'FileUrlTest'
	instVarNames: #(  )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'NetworkTests-Url';
		comment: ''!

(NetworkTestCase
	subclass: 'UrlTest'
	instVarNames: #( url baseUrl )
	classVars: #(  )
	classInstVars: #(  )
	poolDictionaries: #())
		category: 'NetworkTests-Url';
		comment: 'This is the unit test for the class Url. Unit tests are a good way to exercise the functionality of your system in a repeatable and automatic manner. They are therefore recommended if you plan to release anything. For more information, see: 
	- http://www.c2.com/cgi/wiki?UnitTest
	- there is a chapter in the PharoByExample book (http://pharobyexample.org)
	- the sunit class category'!

" Class Implementation for NetworkTestCase"!

" ------------------- Class methods for NetworkTestCase"!

!NetworkTestCase class methodsFor: 'Testing'!
isAbstract
	"Override to true if a TestCase subclass is Abstract and should not have
	TestCase instances built from it"

	^self sunitName = #NetworkTestCase
! !
" Class Implementation for HTTPEncodingTest"!

" ------------------- Instance methods for HTTPEncodingTest"!

!HTTPEncodingTest methodsFor: 'as yet unclassified'!
testEncodeForHTTP

	self assert: 'aa aa éé aa aa' encodeForHTTP = 'aa%20aa%20%C3%A9%C3%A9%20aa%20aa'! !
" Class Implementation for HttpUrlTest"!

" ------------------- Instance methods for HttpUrlTest"!

!HttpUrlTest methodsFor: 'testing'!
testHttps
	self assert: 'https://encrypted.google.com' asUrl scheme = 'https'.! !
" Class Implementation for HierarchicalUrlTest"!

" ------------------- Instance methods for HierarchicalUrlTest"!

!HierarchicalUrlTest methodsFor: 'testing'!
testAsString
	| url |
	url := HierarchicalUrl new
		schemeName: 'ftp'
		authority: 'localhost'
		path: #('path' 'to' 'file')
		query: 'aQuery'.
	self assert: url asString = 'ftp://localhost/path/to/file?aQuery'.! !
" Class Implementation for GenericUrlTest"!

" ------------------- Instance methods for GenericUrlTest"!

!GenericUrlTest methodsFor: 'testing'!
testAsString
	| url |
	url := GenericUrl new schemeName: 'sip' locator: 'foo@bar'.
	self assert: url asString = 'sip:foo@bar'.! !
" Class Implementation for FileUrlTest"!

" ------------------- Instance methods for FileUrlTest"!

!FileUrlTest methodsFor: 'testing'!
testAsString
	| target url |
	target := 'file://localhost/etc/rc.conf'.
	url := target asUrl.
	self assert: url asString = target.
		! !
!FileUrlTest methodsFor: 'testing'!
testMatchingSchemesToSubclasses

	{ { nil. GenericUrl }. "Assume HTTP by default (i.e. when no scheme is provided)"
	{ 'isbn'. GenericUrl }. "Handle unknown Url types with GenericUrl"
	{ 'http'. HttpUrl }.
	{ 'https'. HttpsUrl }.
	{ 'file'. FileUrl }.
	{ 'mailto'. MailtoUrl }.
	{ 'browser'. BrowserUrl } } do: [ :pair | | schemeString urlClassToUse |
		schemeString := pair first.
		urlClassToUse := pair at: 2.
		self assert: (Url urlClassForScheme: schemeString) equals: urlClassToUse ].! !
" Class Implementation for UrlTest"!

" ------------------- Instance methods for UrlTest"!

!UrlTest methodsFor: 'tests'!
testAbsoluteBrowser

	url := Url absoluteFromText: 'browser:bookmarks#mainPart'.

	self assert: url schemeName = 'browser'.
	self assert: url locator = 'bookmarks'.
	self assert:url fragment = 'mainPart'.
	self assert: url class = BrowserUrl.
	! !
!UrlTest methodsFor: 'tests'!
testAbsoluteFILE
	
	url := Url absoluteFromText: 'file:/etc/passwd#foo'.

	self assert: url schemeName = 'file'.
	self assert: url path first = 'etc'.
	self assert: url path size = 2.	
	self assert: url fragment = 'foo'.! !
!UrlTest methodsFor: 'tests'!
testAbsoluteFILE2
	
	url := 'fILE:/foo/bar//zookie/?fakequery/#fragger' asUrl.

	self assert: url schemeName = 'file'.
	self assert: url class = FileUrl.
	self assert: url path first ='foo'.
	self assert: url path size = 5.
	self assert: url fragment = 'fragger'.! !
!UrlTest methodsFor: 'tests'!
testAbsoluteFILE3
	"Just a few selected tests for FileUrl, not complete by any means."


	{'file:'. 'file:/'. 'file://'} do: [:s |
	 	url := FileUrl absoluteFromText: s.
		self assert: (url asString = 'file:///').
		self assert: (url host = '').
		self assert: url isAbsolute].
	
	url := FileUrl absoluteFromText: 'file://localhost/dir/file.txt'.
	self assert: (url asString = 'file://localhost/dir/file.txt').
	self assert: (url host = 'localhost').
	
	url := FileUrl absoluteFromText: 'file://localhost/dir/file.txt'.
	self assert: (url asString = 'file://localhost/dir/file.txt').
	self assert: (url host = 'localhost').
	self assert: url isAbsolute.
	
	url := FileUrl absoluteFromText: 'file:///dir/file.txt'.
	self assert: (url asString = 'file:///dir/file.txt').
	self assert: (url host = '').
	self assert: url isAbsolute.
	
	url := FileUrl absoluteFromText: '/dir/file.txt'.
	self assert: (url asString = 'file:///dir/file.txt').
	self assert: url isAbsolute.
	
	url := FileUrl absoluteFromText: 'dir/file.txt'.
	self assert: (url asString = 'file:///dir/file.txt').
	self deny: url isAbsolute.
	
	url := FileUrl absoluteFromText: 'c:/dir/file.txt'.
	self assert: (url asString = 'file:///c%3A/dir/file.txt').
	self assert: url isAbsolute.
	
	"Only a drive letter doesn't refer to a directory."
	url := FileUrl absoluteFromText: 'c:'.
	self assert: (url asString = 'file:///c%3A/').
	self assert: url isAbsolute.
	
	url := FileUrl absoluteFromText: 'c:/'.
	self assert: (url asString = 'file:///c%3A/').
	self assert: url isAbsolute! !
!UrlTest methodsFor: 'tests'!
testAbsoluteHTTP
	
	url := 'hTTp://chaos.resnet.gatech.edu:8000/docs/java/index.html?A%20query%20#part' asUrl.

	self assert: url schemeName = 'http'.
	self assert: url authority = 'chaos.resnet.gatech.edu'.
	self assert: url path first = 'docs'.
	self assert: url path size = 3.
	self assert: url query = 'A%20query%20'.
	self assert: url fragment = 'part'.! !
!UrlTest methodsFor: 'tests'!
testAbsolutePortErrorFix
	
	self shouldnt: [Url absoluteFromText: 'http://swikis.ddo.jp:8823/'] raise: Error.

	self should: [Url absoluteFromText: 'http://swikis.ddo.jp:-1/'] raise: Error.
	self should: [Url absoluteFromText: 'http://swikis.ddo.jp:65536/'] raise: Error.
	self should: [Url absoluteFromText: 'http://swikis.ddo.jp:auau/'] raise: Error.! !
!UrlTest methodsFor: 'tests'!
testAbsoluteTELNET
	
	url := 'telNet:chaos.resnet.gatech.edu#goo' asUrl.

	self assert: url schemeName = 'telnet'.
	self assert: url locator = 'chaos.resnet.gatech.edu'.
	self assert: url fragment = 'goo'.	
! !
!UrlTest methodsFor: 'tests'!
testCombineWithRelative
	#(#('http://www.rfc1149.net/' 'foo.html' 'http://www.rfc1149.net/foo.html') #('http://www.rfc1149.net/index.html' 'foo.html' 'http://www.rfc1149.net/foo.html') #('http://www.rfc1149.net/devel/' '../sam/' 'http://www.rfc1149.net/sam/') #('http://www.rfc1149.net/devel/index.html' '../sam/' 'http://www.rfc1149.net/sam/')) 
		do: [:a | self assert: (Url combine: a first withRelative: (a at: 2)) = (a at: 3)]! !
!UrlTest methodsFor: 'testing'!
testFromFileNameOrUrlString

	url := Url absoluteFromFileNameOrUrlString: 'asdf'.
	self assert: url schemeName = 'file'.
	self assert: url fragment isNil.
	self assert: url class = FileUrl.

	url := Url absoluteFromFileNameOrUrlString: 'http://209.143.91.36/super/SuperSwikiProj/AAEmptyTest.001.pr'.
	self assert: url schemeName = 'http'.
	self assert: url fragment isNil.
	self assert: url class = HttpUrl.! !
!UrlTest methodsFor: 'tests'!
testRelativeFILE
	
	| url2 |
	baseUrl := 'file:/some/dir#fragment1' asUrl.
	url := baseUrl newFromRelativeText: 'file:../another/dir/#fragment2'.
	self assert: url asString =  'file:///another/dir/#fragment2'.
	
	url := FileUrl absoluteFromText: 'file://localhost/dir/dir2/file.txt'.
	url2 := FileUrl absoluteFromText: 'file://hostname/flip/file.txt'.
	url2 privateInitializeFromText: '../file2.txt' relativeTo: url.
	self assert: (url2 asString = 'file://localhost/dir/file2.txt').
	self assert: (url2 host = 'localhost').
	self assert: url2 isAbsolute.
	
	url := FileUrl absoluteFromText: 'file://localhost/dir/dir2/file.txt'.
	url2 := FileUrl absoluteFromText: 'flip/file.txt'.
	self deny: url2 isAbsolute.
	url2 privateInitializeFromText: '.././flip/file.txt' relativeTo: url.
	self assert: (url2 asString = 'file://localhost/dir/flip/file.txt').
	self assert: (url2 host = 'localhost').
	self assert: url2 isAbsolute.
	
! !
!UrlTest methodsFor: 'tests'!
testRelativeHTTP
	
	baseUrl := 'http://some.where/some/dir?query1#fragment1' asUrl.
	url := baseUrl newFromRelativeText: '../another/dir/?query2#fragment2'.

	self assert: url asString =  'http://some.where/another/dir/?query2#fragment2'.! !
!UrlTest methodsFor: 'tests'!
testUsernameEncodedWithoutPassword
	"Sometimes, weird usernames or passwords are necessary in applications, and, thus, we might receive them in a Url. The @ and the : ar the kind of critical ones."

	#(      "('user' 'pword' 'host' port 'path')"
	('Stéphane rst Pückler' 'leckerEis' 'cottbus.brandenburg' 80 'mein/Zuhause')
	('Jeannde.d''Arc' 'jaiunesécret' 'orleans' 8080 'une/deux/trois')
	('HaXor@roxor:fnac' 'my~Pa$§wert' 'cbase' 42 'do/not_try')
	) do: [:urlParts | |theUrl|
			theUrl := ('http://', (urlParts at: 1) encodeForHTTP, '@', (urlParts at: 3), ':', (urlParts at: 4) printString, '/', (urlParts at: 5)) asUrl.
		self assert: (theUrl schemeName = 'http').
		self assert: (theUrl username = (urlParts at: 1)).
		self assert: (theUrl password isNil).
		self assert: (theUrl authority = (urlParts at: 3)).
		self assert: (theUrl port = (urlParts at: 4)).
		self assert: theUrl path first = ((urlParts at: 5) copyUpTo: $/ )]! !
!UrlTest methodsFor: 'tests'!
testUsernamePassword
	"basic case with a username+password specified"

	url := 'http://user:pword@someserver.blah:8000/root/index.html' asUrl.
	self should: [ url schemeName = 'http' ].
	self should: [ url authority = 'someserver.blah' ].
	self should: [ url port = 8000 ].
	self should: [ url path first = 'root' ].
	self should: [ url username = 'user' ].
	self should: [ url password = 'pword' ].	"basic case for a relative url"
	baseUrl := 'http://anotherserver.blah:9999/somedir/someotherdir/stuff/' asUrl.
	url := 'http://user:pword@someserver.blah:8000/root/index.html' asUrlRelativeTo: baseUrl.
	self should: [ url schemeName = 'http' ].
	self should: [ url authority = 'someserver.blah' ].
	self should: [ url port = 8000 ].
	self should: [ url path first = 'root' ].
	self should: [ url username = 'user' ].
	self should: [ url password = 'pword' ].	"a true relative test that should keep the username and password from the base URL"
	baseUrl := 'http://user:pword@someserver.blah:8000/root/index.html' asUrl.
	url := '/anotherdir/stuff/' asUrlRelativeTo: baseUrl.
	self should: [ url schemeName = 'http' ].
	self should: [ url authority = 'someserver.blah' ].
	self should: [ url port = 8000 ].
	self should: [ url path first = 'anotherdir' ].
	self should: [ url username = 'user' ].
	self should: [ url password = 'pword' ].	"just a username specified"
	url := 'http://user@someserver.blah:8000/root/index.html' asUrl.
	self should: [ url schemeName = 'http' ].
	self should: [ url authority = 'someserver.blah' ].
	self should: [ url port = 8000 ].
	self should: [ url path first = 'root' ].
	self should: [ url username = 'user' ].
	self should: [ url password isNil ].	"the port is not specified"
	url := 'http://user:pword@someserver.blah/root/index.html' asUrl.
	self should: [ url schemeName = 'http' ].
	self should: [ url authority = 'someserver.blah' ].
	self should: [ url port isNil ].
	self should: [ url path first = 'root' ].
	self should: [ url username = 'user' ].
	self should: [ url password = 'pword' ].	"neither a path nor a port is specified"
	url := 'http://user:pword@someserver.blah' asUrl.
	self should: [ url schemeName = 'http' ].
	self should: [ url authority = 'someserver.blah' ].
	self should: [ url port isNil ].
	self should: [ url username = 'user' ].
	self should: [ url password = 'pword' ].	"relative URL where the username+password should be forgotten"
	baseUrl := 'http://user:pword@someserver.blah' asUrl.
	url := 'http://anotherserver.blah' asUrlRelativeTo: baseUrl.
	self should: [ url username isNil ].
	self should: [ url password isNil ]! !
!UrlTest methodsFor: 'tests'!
testUsernamePasswordEncoded2
	"Sometimes, weird usernames or passwords are necessary in applications, and, thus, we might receive them in a Url. The @ and the : ar the kind of critical ones."

	#(      "('user' 'pword' 'host' port 'path')"
	('Stéphane rst Pückler' 'leckerEis' 'cottbus.brandenburg' 80 'mein/Zuhause')
	('Jeannde.d''Arc' 'jaiunesécret' 'orleans' 8080 'une/deux/trois')
	('HaXor@roxor:fnac' 'my~Pa$§wert' 'cbase' 42 'do/not_try')
	) do: [:urlParts | |theUrl|
			theUrl := ('http://', (urlParts at: 1) encodeForHTTP, ':', (urlParts at: 2) encodeForHTTP, '@', (urlParts at: 3), ':', (urlParts at: 4) printString, '/', (urlParts at: 5)) asUrl.
		self assert: (theUrl schemeName = 'http').
		self assert: (theUrl username = (urlParts at: 1)).
		self assert: (theUrl password = (urlParts at: 2)).
		self assert: (theUrl authority = (urlParts at: 3)).
		self assert: (theUrl port = (urlParts at: 4)).
		self assert: theUrl path first = ((urlParts at: 5) copyUpTo: $/ )]! !
!UrlTest methodsFor: 'tests'!
testUsernamePasswordPrinting

	#(	'http://user:pword@someserver.blah:8000/root/index.html'
		'http://user@someserver.blah:8000/root/index.html' 
		'http://user:pword@someserver.blah/root/index.html'
	) do: [ :urlText |
		self should: [ urlText = urlText asUrl asString ] ].

! !
!UrlTest methodsFor: 'tests'!
testUsernamePasswordPrintingEncoded
	
	#('http://F%C3%BCrst%20P%C3%BCckler:leckerEis@cottbus.brandenburg:80/mein/Zuhause'
	'http://Jeannde.d%27Arc:jaiunes%C3%A9cret@orleans:8080/une/deux/trois'
	'http://HaXor%40roxor%3Afnac:my%7EPa%24%C2%A7wert@cbase:42/do/not_try') 
		do: [ :urlText |
			self should: [ urlText = urlText asUrl asString ] ].! !
" Class Extensions "!

" Class initializers "!




" End of Package: NetworkTests"!


