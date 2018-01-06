"
A Uniform Resource Locator.  It specifies the location of a document on the Internet.  The base class is abstract; child classes break different types of URLs down in ways appropriate for that type.
"
Class {
	#name : 'RwUrl',
	#superclass : 'Object',
	#instVars : [
		'fragment'
	],
	#category : 'Rowan-Tode'
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
