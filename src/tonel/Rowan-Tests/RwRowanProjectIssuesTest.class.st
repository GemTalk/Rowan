Class {
	#name : 'RwRowanProjectIssuesTest',
	#superclass : 'RwBrowserToolTest',
	#category : 'Rowan-Tests'
}

{ #category : 'tests' }
RwRowanProjectIssuesTest >> testIssue72 [
	"https://github.com/dalehenrich/Rowan/issues/72"

	| projectName packageNames packageName1 theClass |
	projectName := 'Issue72Project'.
	packageName1 := 'Issue72-Core'.
	packageNames := {packageName1}.
	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment: 'hybrid browser project'.

	theClass := Object
		rwSubclass: 'Issue72Class'
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().
	self assert: theClass rowanPackageName = packageName1.

	"add a packaged method"
	theClass
		rwCompileMethod: 'foo ^''foo'''
		category: '*' , packageName1 asLowercase.

	self 
		should: [
			"add an unpackaged method - expect error"
			Object
				rwCompileMethod: 'bar ^''bar'''
				category: 'accessing' ]
		raise: Error.


	self 
		should: [ 
			[
			"handle notification and abort the operation"
			Object
				rwCompileMethod: 'bar ^''bar'''
				category: 'accessing' ]
					on: RwPerformingUnpackagedEditNotification
					do: [:ex |
						ex resume: false ] ]
		raise: Error.

	[
		"handle notification and complete operation"
		Object
			rwCompileMethod: 'bar ^''bar'''
			category: 'accessing' ]
		on: RwPerformingUnpackagedEditNotification
		do: [:ex |
			ex resume: true.
			self assert: Object new bar = 'bar'.
			Object removeSelector: #'bar'].

]
