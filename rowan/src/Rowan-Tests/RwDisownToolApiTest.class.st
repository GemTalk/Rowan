Class {
	#name : 'RwDisownToolApiTest',
	#superclass : 'RwBrowserToolTest',
	#category : 'Rowan-Tests'
}

{ #category : 'tests' }
RwDisownToolApiTest >> testDisownMethod [
	| projectTools projectName packageNames className packageName theClass fooMethod |
	projectTools := Rowan projectTools.
	projectName := 'DisProject'.
	packageName := 'Disown-Core'.
	packageNames := {packageName}.
	className := 'DisownedClass'.

	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName1
		comment: 'project for testing package disown api'.

	theClass := Object
		rwSubclass: className
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName
		options: #().
	self assert: theClass rowanPackageName = packageName.

	fooMethod := theClass
		rwCompileMethod: 'foo ^''foo'''
		category: '*' , packageName asLowercase.

	self assert: theClass new foo = 'foo'.
	self assert: fooMethod rowanPackageName = packageName.

	projectTools disown
		disownMethod: #foo inClassNamed: className isMeta: false.

	theClass := Rowan globalNamed: className.
	self assert: theClass notNil.
	self assert: theClass new foo = 'foo'.
	self assert: fooMethod rowanPackageName = Rowan unpackagedName.

	projectTools disown
		adoptMethod: #foo inClassNamed: className isMeta: false  intoPackageNamed: packageName.

	theClass := Rowan globalNamed: className.
	self assert: theClass notNil.
	self assert: theClass new foo = 'foo'.
	self assert: fooMethod rowanPackageName = packageName.

]
