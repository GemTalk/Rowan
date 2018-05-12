Class {
	#name : 'RwCloneSymbolDictionaryTest',
	#superclass : 'RwRowanProjectIssuesTest',
	#category : 'Rowan-Tests'
}

{ #category : 'tests' }
RwCloneSymbolDictionaryTest >> testSimpleSystemDictionaryClone [

	| projectName packageName1 className symDictName theOriginalClass theSymbolDict validationBlock | 
	projectName := 'SystemDictionaryClone_Project'.
	packageName1 := 'SystemDictionaryClone-Core'.
	className := 'SystemDictionaryCloneClass'.
	symDictName := self _symbolDictionaryName2.

	self 
		_createLoadedProjectNamed: projectName 
		root: '/tmp/rowanIssuesProject/' 
		symbolDictionaryName: symDictName 
		validate: false.

	self _addPackageNamed: packageName1 toProjectNamed: projectName validate: false.

	theOriginalClass := Object
		rwSubclass: className
		instVarNames: #(iv1)
		classVars: #(CV1)
		classInstVars: #(civ1)
		poolDictionaries: #()
		category: packageName1
		options: #().
	theOriginalClass rwCompileMethod: 'foo ^''foo''' category: 'accessing'.
	theOriginalClass rwCompileMethod: 'iv1 ^iv1' category: 'accessing'.
	theOriginalClass rwCompileMethod: 'initialize iv1:=3' category: 'initialization'.

	theOriginalClass class rwCompileMethod: 'bar ^''bar''' category: 'accessing'.
	theOriginalClass class rwCompileMethod: 'cv1 ^CV1' category: 'accessing'.
	theOriginalClass class rwCompileMethod: 'civ1 ^civ1' category: 'accessing'.
	theOriginalClass class rwCompileMethod: 'initialize civ1:=1. CV1:=2.' category: 'initialization'.
	theOriginalClass class rwCompileMethod: 'new ^ self basicNew initialize; yourself' category: 'instance creation'.

	theOriginalClass initialize.
	validationBlock := [:aClass |
		self assert: aClass cv1 = 2.
		self assert: aClass civ1 = 1.
		self assert: aClass bar = 'bar'.
		self assert: aClass new iv1 = 3.
		self assert: aClass new foo = 'foo'.
	].

	validationBlock value: theOriginalClass.
	theSymbolDict := Rowan globalNamed: symDictName.

]
