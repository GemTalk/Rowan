Class {
	#name : 'RwRowanProjectIssuesTest',
	#superclass : 'RwBrowserToolTest',
	#category : 'Rowan-Tests'
}

{ #category : 'tests' }
RwRowanProjectIssuesTest >> testIssue72 [
	"https://github.com/dalehenrich/Rowan/issues/72"

	| projectName packageNames packageName1 theClass theInstance |
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

	"add an unpackaged method"
	Object
		rwCompileMethod: 'bar ^''bar'''
		category: 'accessing'

]
