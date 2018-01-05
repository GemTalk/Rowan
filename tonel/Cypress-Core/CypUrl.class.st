"
No class-specific documentation for CypUrl, hierarchy is: 
Object
  CypUrl

"
Class {
	#name : 'CypUrl',
	#superclass : 'Object',
	#instVars : [
		'fragment'
	],
	#category : 'Cypress-Core'
}

{ #category : 'public' }
CypUrl class >> fromString: aString [
	"trim surrounding whitespace"

	| remainder index scheme fragment newUrl |
	remainder := aString trimSeparators.

	"extract the fragment, if any"
	index := remainder indexOf: $#.
	index > 0
		ifTrue: 
			[fragment := remainder copyFrom: index + 1 to: remainder size.
			remainder := remainder copyFrom: 1 to: index - 1].

	"choose class based on the scheme name, and let that class do the bulk of the parsing"
	scheme := self schemeNameForString: remainder.
	newUrl := (self urlClassForScheme: scheme) new
				privateInitializeFromText: remainder.
	newUrl privateFragment: fragment.
	^newUrl
]

{ #category : 'private' }
CypUrl class >> schemeName [
	"When searching for a class to handle a particular scheme, make sure that Url classes never match by default. This is so that abstract Url classes e.g. HierarchicalUrl can be iterated over, but will not be selected"

	^nil
]

{ #category : 'private' }
CypUrl class >> schemeNameForString: aString [
	"Get the scheme name from a string, or return nil if it's not specified. 
	Used in internal parsing routines - an outsider may as well use asUrl. 
	Return scheme in lowercases."
	"Url schemeNameForString: 'http://www.yahoo.com'"
	"Url schemeNameForString: '/etc/passwed'"
	"Url schemeNameForString: '/etc/testing:1.2.3'"

	| index schemeName |
	index := aString indexOf: $: ifAbsent: [^nil].
	schemeName := aString copyFrom: 1 to: index - 1.
	(schemeName allSatisfy: [:each | each isLetter]) ifFalse: [^nil].
	^schemeName asLowercase
]

{ #category : 'private' }
CypUrl class >> urlClassForScheme: scheme [
  ^ self allSubclasses
    detect: [ :urlClass | urlClass schemeName = scheme ]
    ifNone: [ CypGenericUrl ]
]

{ #category : 'Accessing' }
CypUrl >> fragment [
	^fragment

]

{ #category : 'Updating' }
CypUrl >> fragment: newValue [
	fragment := newValue

]

{ #category : 'private' }
CypUrl >> privateFragment: aString [

	fragment := aString
]
