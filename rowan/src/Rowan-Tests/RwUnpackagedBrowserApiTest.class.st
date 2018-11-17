Class {
	#name : 'RwUnpackagedBrowserApiTest',
	#superclass : 'RwBrowserToolTest',
	#category : 'Rowan-Tests'
}

{ #category : 'private' }
RwUnpackagedBrowserApiTest >> _testClass [

	^ Rowan globalNamed: self _testClassName
]

{ #category : 'private' }
RwUnpackagedBrowserApiTest >> _testClassName [

	^  'Issue364_class'
]

{ #category : 'running' }
RwUnpackagedBrowserApiTest >> setUp [
	"test class will be removed along with symbol dictionary during teardown"

	| symDict |
	super setUp.
	Rowan image newOrExistingSymbolDictionaryNamed: self _symbolDictionaryName1.
	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	Object subclass: self _testClassName
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: symDict
		options: #()
]

{ #category : 'tests' }
RwUnpackagedBrowserApiTest >> testIssue263 [

	"https://github.com/GemTalk/Rowan/issues/263"

	"Add extension method to an unpackaged class"

	| projectName  packageName projectDefinition projectSetDefinition audit testClass |

	projectName := 'Issue263'.
	packageName := 'Issue263-Extension'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName;
		yourself.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	testClass := self _testClass.
	testClass
		rwCompileMethod: 'bar ^''bar'''
		category: '*' , packageName asLowercase.

"validate"
	self assert: (testClass compiledMethodAt: 'bar' otherwise: nil) notNil.

"audit"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.

"load -- effectively unload extension methods"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	self assert: (testClass compiledMethodAt: 'bar' otherwise: nil) isNil.

"audit"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.
]

{ #category : 'tests' }
RwUnpackagedBrowserApiTest >> testAddUnpackagedMethod [

	"https://github.com/GemTalk/Rowan/issues/364"

	"Add unpackaged method"

	| audit |

	Object
		rwCompileMethod: 'bar ^''bar'''
		category: 'issue 364'.

"validate"
	self assert: (Object compiledMethodAt: 'bar' otherwise: nil) notNil.

	Object rwRemoveSelector: #bar.

"validate"
	self assert: (Object compiledMethodAt: 'bar' otherwise: nil) isNil.

"audit"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: 'Rowan') isEmpty.
]

{ #category : 'tests' }
RwUnpackagedBrowserApiTest >> testMovePackagedMethod [

	"https://github.com/GemTalk/Rowan/issues/364"

	| projectName  packageName1 packageName2 projectDefinition projectSetDefinition audit |

	projectName := 'Issue364'.
	packageName1 := 'Issue364-Extension-1'.
	packageName2 := 'Issue364-Extension-2'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName1;
		addPackageNamed: packageName2;
		defaultSymbolDictName: 'Globals';
		yourself.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	Object
		rwCompileMethod: 'bar ^''bar'''
		category: '*' , packageName1 asLowercase.

"validate"
	self assert: (Object compiledMethodAt: 'bar' otherwise: nil) notNil.
	self assert: (Object categoryOfSelector: #bar) = '*' , packageName1 asLowercase.

	Object rwMoveMethod: #bar toCategory: '*' , packageName2 asLowercase.

"validate"
	self assert: (Object compiledMethodAt: 'bar' otherwise: nil) notNil.
	self assert: (Object categoryOfSelector: #bar) = '*' , packageName2 asLowercase.

"load -- effectively unload extension methods"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	self assert: (Object compiledMethodAt: 'bar' otherwise: nil) isNil.

"audit"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.
]

{ #category : 'tests' }
RwUnpackagedBrowserApiTest >> testMoveUnpackagedMethod [

	"https://github.com/GemTalk/Rowan/issues/364"

	"Add unpackaged method"

	| audit x |

	Object
		rwCompileMethod: 'bar ^''bar'''
		category: #'issue 364'.

"validate"
	self assert: (Object compiledMethodAt: 'bar' otherwise: nil) notNil.
	self assert: (x := Object categoryOfSelector: #bar) = #'issue 364'.

	Object rwMoveMethod: #bar toCategory: #'issue 364 other'.

"validate"
	self assert: (Object compiledMethodAt: 'bar' otherwise: nil) notNil.
	self assert: (x := Object categoryOfSelector: #bar) = #'issue 364 other'.

	Object rwRemoveSelector: #bar.

"validate"
	self assert: (Object compiledMethodAt: 'bar' otherwise: nil) isNil.

"audit"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: 'Rowan') isEmpty.
]
