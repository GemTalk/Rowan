Class {
	#name : 'RwRowanProjectIssuesTest',
	#superclass : 'RwBrowserToolTest',
	#category : 'Rowan-Tests'
}

{ #category : 'tests' }
RwRowanProjectIssuesTest >> testIssue72_addMethod [
	"https://github.com/dalehenrich/Rowan/issues/72"

	| x |
	self 
		should: [
			"add an unpackaged method - expect error"
			Object
				rwCompileMethod: 'bar ^''bar'''
				category: 'accessing' ]
		raise: Error.

	(Object compiledMethodAt: #bar environmentId: 0 otherwise: nil)
		ifNotNil: [ Object removeSelector: #'bar' ].

	[
		"handle notification and abort the operation"
		Object
			rwCompileMethod: 'bar ^''bar'''
			category: 'accessing' ]
		on: RwPerformingUnpackagedEditNotification
		do: [:ex | "abort"].
	self assert: (x := Object compiledMethodAt: #bar environmentId: 0 otherwise: nil) isNil.

	[
		"handle notification and complete operation"
		Object
			rwCompileMethod: 'bar ^''bar'''
			category: 'accessing' ]
		on: RwPerformingUnpackagedEditNotification
		do: [:ex | ex resume ].

	"ensure that the new method is not packaged"
	Rowan image 
		loadedMethod: #bar 
		inClassNamed: 'Object' 
		isMeta: false 
		ifFound: [self assert: false description: 'unexpected loaded method' ] 
		ifAbsent: ["expected"].

	self assert: Object new bar = 'bar'.
	Object removeSelector: #'bar'

]

{ #category : 'tests' }
RwRowanProjectIssuesTest >> testIssue72_removeClass [
	"https://github.com/dalehenrich/Rowan/issues/72"

	"nothing special really needed when adding/updating an uppackaged class, just use the standard class creation protocol"

	| className browser |
	className := 'Issue72TestClass'.
	Object subclass: className
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: self _symbolDictionary
		options: #().
	self assert: (Rowan image objectNamed: className) notNil.

	browser := Rowan projectTools browser.

	self 
		should: [ browser removeClassNamed: className ]
		raise: Error.
	self assert: (Rowan image objectNamed: className) notNil.

	[
		"handle notification and abort the remove"
		browser removeClassNamed: className ]
			on: RwPerformingUnpackagedEditNotification
			do: [:ex | "abort"].
	self assert: (Rowan image objectNamed: className) notNil.

	[
		"handle notification and complete removal"
		browser removeClassNamed: className  ]
			on: RwPerformingUnpackagedEditNotification
			do: [:ex | ex resume ].
	self assert: (Rowan image objectNamed: className) isNil.

]

{ #category : 'tests' }
RwRowanProjectIssuesTest >> testIssue72_removeClassWithExtensions [
	"https://github.com/dalehenrich/Rowan/issues/72"

	"nothing special really needed when adding/updating an uppackaged class, just use the standard class creation protocol"

	| className |
	className := 'Issue72TestClass'.
	Object subclass: className
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: self _symbolDictionary
		options: #().

	Rowan projectTools browser removeClassNamed: className.

]

{ #category : 'tests' }
RwRowanProjectIssuesTest >> testIssue72_removeMethod [
	"https://github.com/dalehenrich/Rowan/issues/72"

	[
		"add unpackaged method"
		Object
			rwCompileMethod: 'bar ^''bar'''
			category: 'accessing' ]
		on: RwPerformingUnpackagedEditNotification
		do: [:ex | ex resume ].
	self assert: Object new bar = 'bar'.

	self 
		should: [
			"attempt to remove existing unpackaged method - expect error"
			Object rwRemoveSelector: #bar ]
		raise: Error.
	self assert: Object new bar = 'bar'.

	[
		"handle notification and abort the remove"
		Object rwRemoveSelector: #bar ]
		on: RwPerformingUnpackagedEditNotification
		do: [:ex | "abort"].
	self assert: Object new bar = 'bar'.

	[
		"handle notification and complete removal"
		Object rwRemoveSelector: #bar  ]
		on: RwPerformingUnpackagedEditNotification
		do: [:ex | ex resume ].
	self should: [ Object new bar ] raise: MessageNotUnderstood.

]

{ #category : 'tests' }
RwRowanProjectIssuesTest >> testIssue72_updateMethod [
	"https://github.com/dalehenrich/Rowan/issues/72"

	[
		"add unpackaged method"
		Object
			rwCompileMethod: 'bar ^''bar'''
			category: 'accessing' ]
		on: RwPerformingUnpackagedEditNotification
		do: [:ex | ex resume ].
	self assert: Object new bar = 'bar'.

	self 
		should: [
			"attempt to update existing unpackaged method - expect error"
			Object
				rwCompileMethod: 'bar ^''foo'''
				category: 'accessing' ]
		raise: Error.
	self assert: Object new bar = 'bar'.

	[
		"handle notification and abort the operation"
		Object
			rwCompileMethod: 'bar ^''foo'''
			category: 'accessing' ]
		on: RwPerformingUnpackagedEditNotification
		do: [:ex | "abort"].
	self assert: Object new bar = 'bar'.

	[
		"handle notification and complete operation"
		Object
			rwCompileMethod: 'bar ^''foo'''
			category: 'accessing' ]
		on: RwPerformingUnpackagedEditNotification
		do: [:ex | ex resume ].
	self assert: Object new bar = 'foo'.

	Object removeSelector: #'bar'

]
