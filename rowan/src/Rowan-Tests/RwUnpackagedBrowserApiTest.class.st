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

{ #category : 'private' }
RwUnpackagedBrowserApiTest >> _testPackagedClassName [

	^  'Issue364_class_packaged'
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
RwUnpackagedBrowserApiTest >> testAddUnpackagedClassMethod [

	"https://github.com/GemTalk/Rowan/issues/547"

	"Add unpackaged method to an upackaged class"

	| testClass audit |

	testClass := self _testClass.
	[ testClass class
		rwCompileMethod: 'bar ^''bar'''
		category: 'issue 364' ]
		on: RwPerformingUnpackagedEditNotification
		do: [:ex | ex resume ].	

"validate"
	self assert: (testClass class compiledMethodAt: 'bar' otherwise: nil) notNil.

	[ testClass class rwRemoveSelector: #bar ]
		on: RwPerformingUnpackagedEditNotification
		do: [:ex | ex resume ].	

"validate"
	self assert: (testClass compiledMethodAt: 'bar' otherwise: nil) isNil.

"audit"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: 'Rowan') isEmpty.
]

{ #category : 'tests' }
RwUnpackagedBrowserApiTest >> testAddUnpackagedMethod [

	"https://github.com/GemTalk/Rowan/issues/364"

	"Add unpackaged method to an upackaged class"

	| testClass audit |

	testClass := self _testClass.
	[ testClass
		rwCompileMethod: 'bar ^''bar'''
		category: 'issue 364' ]
		on: RwPerformingUnpackagedEditNotification
		do: [:ex | ex resume ].	

"validate"
	self assert: (testClass compiledMethodAt: 'bar' otherwise: nil) notNil.

	[ testClass rwRemoveSelector: #bar ]
		on: RwPerformingUnpackagedEditNotification
		do: [:ex | ex resume ].	

"validate"
	self assert: (testClass compiledMethodAt: 'bar' otherwise: nil) isNil.

"audit"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: 'Rowan') isEmpty.
]

{ #category : 'tests' }
RwUnpackagedBrowserApiTest >> testIssue263 [

	"https://github.com/GemTalk/Rowan/issues/263"

	"Add extension method to an unpackaged class"

	| projectName  packageName projectDefinition projectSetDefinition audit testClass |

	projectName := 'Issue263_02'.
	packageName := 'Issue263-Extension'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

	self assert: (projectDefinition projectDefinitionSourceProperty = RwLoadedProject _projectUnknownDefinitionSourceValue).

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
RwUnpackagedBrowserApiTest >> testIssue420_A [

	"https://github.com/GemTalk/Rowan/issues/420"

	"move method from packaged category to another packaged category (different package) (class is unpackaged)"

	| projectName  packageName1 packageName2 projectDefinition projectSetDefinition audit testClass symDict registry |

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
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	testClass := self _testClass.
	testClass addCategory: '*' , packageName1 asLowercase.
	testClass
		rwCompileMethod: 'bar ^''bar'''
		category: '*' , packageName1 asLowercase.

"validate"
	self assert: (testClass compiledMethodAt: 'bar' otherwise: nil) notNil.
	self assert: (testClass categoryOfSelector: #bar) = ('*' , packageName1 asLowercase) asSymbol.
"registry check"
	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 2.
	self assert: registry classRegistry size = 0.
	self assert: registry classExtensionRegistry size = 1.
	self assert: registry methodRegistry size = 1.

"move"
	testClass addCategory: '*' , packageName2 asLowercase.
	testClass rwMoveMethod: #bar toCategory: '*' , packageName2 asLowercase.

"validate"
	self assert: (testClass compiledMethodAt: 'bar' otherwise: nil) notNil.
	self assert: (testClass categoryOfSelector: #bar) = ('*' , packageName2 asLowercase) asSymbol.
"registry check"
	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 2.
	self assert: registry classRegistry size = 0.
	self assert: registry classExtensionRegistry size = 1.
	self assert: registry methodRegistry size = 1.

"load -- effectively unload extension methods"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	self assert: (testClass compiledMethodAt: 'bar' otherwise: nil) isNil.

"audit"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.
"registry check"
	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 2.
	self assert: registry classRegistry size = 0.
	self assert: registry classExtensionRegistry size = 0.
	self assert: registry methodRegistry size = 0.
]

{ #category : 'tests' }
RwUnpackagedBrowserApiTest >> testIssue420_B [

	"https://github.com/GemTalk/Rowan/issues/420"

	"move method from extension category to packaged category (class is packaged)"

	| projectName  packageName1 packageName2 projectDefinition projectSetDefinition audit testClass symDict registry |

	projectName := 'Issue364'.
	packageName1 := 'Issue364-Core-1'.
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
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	testClass := Object rwSubclass: self _testPackagedClassName
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		category: packageName1
		options: #().

	testClass addCategory: '*' , packageName2 asLowercase.
	testClass
		rwCompileMethod: 'bar ^''bar'''
		category: '*' , packageName2 asLowercase.

"validate"
	self assert: (testClass compiledMethodAt: 'bar' otherwise: nil) notNil.
	self assert: (testClass categoryOfSelector: #bar) = ('*' , packageName2 asLowercase) asSymbol.
"registry check"
	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 2.
	self assert: registry classRegistry size = 1.
	self assert: registry classExtensionRegistry size = 1.
	self assert: registry methodRegistry size = 1.

"move"
	testClass addCategory: 'accessing'.
	testClass rwMoveMethod: #bar toCategory: 'accessing'.

"validate"
	self assert: (testClass compiledMethodAt: 'bar' otherwise: nil) notNil.
	self assert: (testClass categoryOfSelector: #bar) = #'accessing'.
"registry check"
	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 2.
	self assert: registry classRegistry size = 1.
	self assert: registry classExtensionRegistry size = 0.
	self assert: registry methodRegistry size = 1.

"load -- effectively unload extension methods"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	self assert: (Rowan globalNamed: self _testPackagedClassName) isNil.

"audit"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.

"registry check"
	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 2.
	self assert: registry classRegistry size = 0.
	self assert: registry classExtensionRegistry size = 0.
	self assert: registry methodRegistry size = 0.
]

{ #category : 'tests' }
RwUnpackagedBrowserApiTest >> testIssue420_C [

	"https://github.com/GemTalk/Rowan/issues/420"

	"move method from packaged category to unpackaged category (class is unpackaged)"

	| projectName  packageName1 packageName2 projectDefinition projectSetDefinition audit testClass symDict registry |

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
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	testClass := self _testClass.
	testClass addCategory: '*' , packageName1 asLowercase.
	testClass
		rwCompileMethod: 'bar ^''bar'''
		category: '*' , packageName1 asLowercase.

"validate"
	self assert: (testClass compiledMethodAt: 'bar' otherwise: nil) notNil.
	self assert: (testClass categoryOfSelector: #bar) = ('*' , packageName1 asLowercase) asSymbol.
"registry check"
	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 2.
	self assert: registry classRegistry size = 0.
	self assert: registry classExtensionRegistry size = 1.
	self assert: registry methodRegistry size = 1.

"move"
	testClass addCategory: 'accessing'.
	[ testClass rwMoveMethod: #bar toCategory:  'accessing' ]
		on: RwPerformingUnpackagedEditNotification
		do: [:ex | ex resume ].	

"validate"
	self assert: (testClass compiledMethodAt: 'bar' otherwise: nil) notNil.
	self assert: (testClass categoryOfSelector: #bar) = 'accessing'  asSymbol.
"audit"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.
"registry check"
	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 2.
	self assert: registry classRegistry size = 0.
	self assert: registry classExtensionRegistry size = 0.
	self assert: registry methodRegistry size = 0.

"load -- effectively unload extension methods"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	self assert: (testClass compiledMethodAt: 'bar' otherwise: nil) notNil.
	[ testClass rwRemoveSelector: #bar ]
			on: RwPerformingUnpackagedEditNotification
			do: [:ex | ex resume ].	

"validate"
	self assert: (testClass compiledMethodAt: 'bar' otherwise: nil) isNil.

"audit"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.
]

{ #category : 'tests' }
RwUnpackagedBrowserApiTest >> testIssue420_D [

	"https://github.com/GemTalk/Rowan/issues/420"

	"move method from unpackaged category to packaged category (class is unpackaged)"

	| projectName  packageName1 projectDefinition projectSetDefinition audit testClass symDict registry |

	projectName := 'Issue364'.
	packageName1 := 'Issue364-Extension-1'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName1;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	testClass := self _testClass.
	testClass addCategory: 'accessing'.
	[ testClass
		rwCompileMethod: 'bar ^''bar'''
		category: 'accessing' ]
			on: RwPerformingUnpackagedEditNotification
			do: [:ex | ex resume ].	

"validate"
	self assert: (testClass compiledMethodAt: 'bar' otherwise: nil) notNil.
	self assert: (testClass categoryOfSelector: #bar) = 'accessing' asSymbol.
"audit"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.
"registry check"
	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.
	self assert: registry classRegistry size = 0.
	self assert: registry classExtensionRegistry size = 0.
	self assert: registry methodRegistry size = 0.

"move"
	testClass addCategory: '*' , packageName1 asLowercase.
	testClass rwMoveMethod: #bar toCategory: '*' , packageName1 asLowercase.	

"validate"
	self assert: (testClass compiledMethodAt: 'bar' otherwise: nil) notNil.
	self assert: (testClass categoryOfSelector: #bar) = ('*' , packageName1 asLowercase) asSymbol.
"audit"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.
"registry check"
	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.
	self assert: registry classRegistry size = 0.
	self assert: registry classExtensionRegistry size = 1.
	self assert: registry methodRegistry size = 1.

"load -- effectively unload extension methods"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	self assert: (testClass compiledMethodAt: 'bar' otherwise: nil) isNil.
"audit"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.
"registry check"
	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.
	self assert: registry classRegistry size = 0.
	self assert: registry classExtensionRegistry size = 0.
	self assert: registry methodRegistry size = 0.
]

{ #category : 'tests' }
RwUnpackagedBrowserApiTest >> testIssue420_E [

	"https://github.com/GemTalk/Rowan/issues/420"

	"move method from unpackaged category to category in another unpackaged category (class is unpackaged)"

	| projectName  packageName1 projectDefinition projectSetDefinition audit testClass symDict registry |

	projectName := 'Issue364'.
	packageName1 := 'Issue364-Extension-1'.

	{projectName}
		do: [ :pn | 
			(Rowan image loadedProjectNamed: pn ifAbsent: [  ])
				ifNotNil: [ :loadedProject | Rowan image _removeLoadedProject: loadedProject ] ].

"create project"
	projectDefinition := (RwProjectDefinition
		newForGitBasedProjectNamed: projectName)
		addPackageNamed: packageName1;
		defaultSymbolDictName: self _symbolDictionaryName1;
		yourself.

"load"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

	testClass := self _testClass.
	testClass addCategory: 'accessing'.
	[ testClass
		rwCompileMethod: 'bar ^''bar'''
		category: 'accessing' ]
			on: RwPerformingUnpackagedEditNotification
			do: [:ex | ex resume ].	

"validate"
	self assert: (testClass compiledMethodAt: 'bar' otherwise: nil) notNil.
	self assert: (testClass categoryOfSelector: #bar) = 'accessing' asSymbol.
"registry check"
	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.
	self assert: registry classRegistry size = 0.
	self assert: registry classExtensionRegistry size = 0.
	self assert: registry methodRegistry size = 0.

"move"
	testClass addCategory: 'other accessing'.
	testClass rwMoveMethod: #bar toCategory: 'other accessing'.	

"validate"
	self assert: (testClass compiledMethodAt: 'bar' otherwise: nil) notNil.
	self assert: (testClass categoryOfSelector: #bar) = #'other accessing'.
"registry check"
	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.
	self assert: registry classRegistry size = 0.
	self assert: registry classExtensionRegistry size = 0.
	self assert: registry methodRegistry size = 0.

"load -- effectively unload extension methods"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	[ testClass rwRemoveSelector: #bar ]
			on: RwPerformingUnpackagedEditNotification
			do: [:ex | ex resume ].	
	self assert: (testClass compiledMethodAt: 'bar' otherwise: nil) isNil.

"audit"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.
"registry check"
	symDict := Rowan globalNamed: self _symbolDictionaryName1.
	registry := symDict rowanSymbolDictionaryRegistry.

	self assert: registry packageRegistry size = 1.
	self assert: registry classRegistry size = 0.
	self assert: registry classExtensionRegistry size = 0.
	self assert: registry methodRegistry size = 0.
]

{ #category : 'tests' }
RwUnpackagedBrowserApiTest >> testMoveUnpackagedMethod [

	"https://github.com/GemTalk/Rowan/issues/364"

	"Add unpackaged method"

	| audit x testClass |

	testClass := self _testClass.
	[ testClass
		rwCompileMethod: 'bar ^''bar'''
		category: 'issue 364' ]
		on: RwPerformingUnpackagedEditNotification
		do: [:ex | ex resume ].	

"validate"
	self assert: (testClass compiledMethodAt: 'bar' otherwise: nil) notNil.
	self assert: (x := testClass categoryOfSelector: #bar) = #'issue 364'.

	testClass addCategory: 'issue 364 other'.
	testClass rwMoveMethod: #bar toCategory: 'issue 364 other'.	

"validate"
	self assert: (testClass compiledMethodAt: 'bar' otherwise: nil) notNil.
	self assert: (x := testClass categoryOfSelector: #bar) = #'issue 364 other'.

	[ testClass rwRemoveSelector: #bar  ]
		on: RwPerformingUnpackagedEditNotification
		do: [:ex | ex resume ].	

"validate"
	self assert: (testClass compiledMethodAt: 'bar' otherwise: nil) isNil.

"audit"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: 'Rowan') isEmpty.
]

{ #category : 'tests' }
RwUnpackagedBrowserApiTest >> testUpdateUnpackagedClassComment [

	"https://github.com/GemTalk/Rowan/issues/364"

	"Add comment to unpackaged class"

	| audit testClass |

	testClass := self _testClass.
	[ testClass
		rwComment: 'a comment' ]
		on: RwPerformingUnpackagedEditNotification
		do: [:ex | ex resume ].	

"validate"
	self assert: testClass comment = 'a comment'.

"audit"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: 'Rowan') isEmpty.
]
