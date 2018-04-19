"
All Cypress classes are private to GemStone and are likely to be removed in a future release.
"
Class {
	#name : 'CypressAbstractFileUrl',
	#superclass : 'CypressFileUrl',
	#category : 'Cypress-PackageManagement'
}

{ #category : 'instance creation' }
CypressAbstractFileUrl class >> absoluteFromText: aString [
	"Return a URL from a string and handle
	a String without a scheme as a FileUrl."

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
	^newUrl

]

{ #category : 'parsing' }
CypressAbstractFileUrl class >> urlClassForScheme: scheme [

	scheme isNil ifTrue: [^CypressFileUrl].
	^super urlClassForScheme: scheme

]

{ #category : 'accessing' }
CypressAbstractFileUrl >> codeFormat [

	^self subclassResponsibility: #codeFormat.

]

{ #category : 'private' }
CypressAbstractFileUrl >> fileUtils [
  ^ CypressFileUtilities current

]

{ #category : 'testing' }
CypressAbstractFileUrl >> isStrict [

	^self subclassResponsibility: #isStrict.

]

{ #category : 'accessing' }
CypressAbstractFileUrl >> repositoryClass [

	^CypressFileSystemRepository

]
