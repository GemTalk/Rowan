"
A Uniform Resource Locator.  It specifies the location of a document on the Internet.  The base class is abstract; child classes break different types of URLs down in ways appropriate for that type.

The classes in this hierarchy are specificaly designed for parsing URLS for Rowan source code repositories:

 file:/opt/git/shared/repos/rowan/sample/repository
 http://gsdevkit.github.io/GsDevKit_home/rowan/Sample.ston
 https://raw.githubusercontent.com/dalehenrich/sample/master/specs/Sample.ston

 github://GsDevKit/GsDevKit:master/repository
 filetree:///opt/git/shared/repos/rowan/sample/repository
 tonel:/opt/git/shared/repos/rowan/sample/repository
 gitfiletree://gitlab.com/GsDevKit/GsDevKit:master/repository

 cypress:/opt/git/shared/repos/rowan/sample/repository/
 metacello://dkh:pass@gitlab.ferlicot.fr:3456/Projet/Bazard:dev/src
 metacello://git@git.gemtalksystems.com/btree340:dev/repository
 metacello://git@github.com/GsDevKit/GsDevKit:350/repository
 metacello://github.com/GsDevKit/GsDevKit:350/repository


The file:, http: and https: schemes should conform to the standard specs. ZnUrl is used for parsing http: and https: urls.

The github:, filetree:, gitfiletree: and tonel: schemes are supported for backward compatibility with schemes that have historically been used to identify Metacello repository urls.

The cypress: and metacello: schemes are new and intended to be used moving forward with Metacello and Rowan.

The cypress: url is used to indicate that path to a Cypress-style repository, i.e., a disk-based format for storing Smalltalk packages in filetree or tonel format. A cypress: url does not define the specific repository type (filetree or tonel). The type of the repository is encoded in a .filetree file located in the directory specified by pathString of the url.

The metacello: scheme is based on Thierry Goubier's gitfiletree url[1]. The original github: scheme cannot be used for arbitrary git servers or git servers using non-standard ports or requiring usernames and passwords. Thierry's scheme handles the full range of possibilites.

[1] https://github.com/dalehenrich/filetree/blob/734eed46ea57ebf5e24e5d935768bd49727fc22f/repository/MonticelloFileTree-Git.package/MCFileTreeGitRepository.class/class/basicFromUrl..st
"
Class {
	#name : 'RwUrl',
	#superclass : 'Object',
	#instVars : [
		'fragment'
	],
	#category : 'Rowan-Url'
}

{ #category : 'instance creation' }
RwUrl class >> fromString: aString [
	"Return a URL from a string and handle
	a String without a scheme as a HttpUrl."

	"Url fromString: 'http://chaos.resnet.gatech.edu:8000/docs/java/index.html?A%20query%20#part'" 
	"Url fromString: 'msw://chaos.resnet.gatech.edu:9000/testbook?top'"
	"Url fromString: 'telnet:chaos.resnet.gatech.edu'"
	"Url fromString: 'file:/etc/passwd'"

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
	(scheme = 'http' or: [ scheme = 'https' ])
		ifTrue: [ ^ ZnUrl fromString: aString ].
	newUrl := (self urlClassForScheme: scheme) new privateInitializeFromText: remainder.
	newUrl privateFragment: fragment.
	^newUrl

]

{ #category : 'constants' }
RwUrl class >> schemeName [
	"When searching for a class to handle a particular scheme, make sure that Url classes never match by default. This is so that abstract Url classes e.g. HierarchicalUrl can be iterated over, but will not be selected"

	^ nil.

]

{ #category : 'parsing' }
RwUrl class >> schemeNameForString: aString [
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
	^ schemeName asLowercase

]

{ #category : 'parsing' }
RwUrl class >> urlClassForScheme: scheme [

	^ self allSubclasses
				detect: [ :urlClass | urlClass schemeName = scheme ]
				ifNone: [ RwGenericUrl ].

]

{ #category : 'converting' }
RwUrl >> asString [

	^self printString

]

{ #category : 'converting' }
RwUrl >> asUrl [
	^self

]

{ #category : 'accessing' }
RwUrl >> authority [
	^''

]

{ #category : 'fragment' }
RwUrl >> fragment [
	^fragment

]

{ #category : 'accessing' }
RwUrl >> pathFor: coll [
  | ans sz |
  ans := WriteStreamPortable on: String new.
  sz := coll size.
  1 to: sz do: [ :index | 
    | pathElem |
    pathElem := coll at: index.
    ans nextPutAll: pathElem.
    index < sz
      ifTrue: [ ans nextPut: $/ ] ].
  ^ ans contents
]

{ #category : 'printing' }
RwUrl >> printOn: aStream [

	^self subclassResponsibility: #printOn:

]

{ #category : 'fragment' }
RwUrl >> privateFragment: aString [
	fragment := aString

]

{ #category : 'parsing' }
RwUrl >> privateInitializeFromText: aString [

	^self subclassResponsibility: #privateInitializeFromText:

]

{ #category : 'parsing' }
RwUrl >> privateInitializeFromText: aString relativeTo: aUrl [
	"initialize from the given string, as a relative URL.  aString will have had the scheme name removed, if it was present to begin with.  If it was, then the scheme name was the same as the receiver's scheme name"

	"by default, just do regular initialization"
	^self privateInitializeFromText: aString

]

{ #category : 'classification' }
RwUrl >> scheme [
	"return a string with the scheme of this URL.  For instance, HTTP"

	^self subclassResponsibility: #scheme

]

{ #category : 'classification' }
RwUrl >> schemeName [
	"return a lowercase string with the scheme of this URL.  For instance, 'http'"

	^self subclassResponsibility: #schemeName

]
