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
		defaultSymbolDictName: self _symbolDictionaryName1;
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
RwUnpackagedBrowserApiTest >> testIssue420_A [

	"https://github.com/GemTalk/Rowan/issues/420"

	"move method from one extension category to another in an unpackaged class"

	| projectName  packageName1 packageName2 projectDefinition projectSetDefinition audit testClass |

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

"move"
	testClass addCategory: '*' , packageName2 asLowercase.
	testClass rwMoveMethod: #bar toCategory: '*' , packageName2 asLowercase.

"validate"
	self assert: (testClass compiledMethodAt: 'bar' otherwise: nil) notNil.
	self assert: (testClass categoryOfSelector: #bar) = ('*' , packageName2 asLowercase) asSymbol.

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
RwUnpackagedBrowserApiTest >> testIssue420_B [

	"https://github.com/GemTalk/Rowan/issues/420"

	"move method from hypbrid package category to packaged class package category"

	| projectName  packageName1 packageName2 projectDefinition projectSetDefinition audit testClass |

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

"move"
	testClass addCategory: 'accessing'.
	testClass rwMoveMethod: #bar toCategory: 'accessing'.

"validate"
	self assert: (testClass compiledMethodAt: 'bar' otherwise: nil) notNil.
	self assert: (testClass categoryOfSelector: #bar) = #'accessing'.

"load -- effectively unload extension methods"
	projectSetDefinition := RwProjectSetDefinition new.
	projectSetDefinition addDefinition: projectDefinition.
	Rowan projectTools load loadProjectSetDefinition: projectSetDefinition.

"validate"
	self assert: (Rowan globalNamed: self _testPackagedClassName) isNil.

"audit"
	self assert: (audit := Rowan projectTools audit auditForProjectNamed: projectName) isEmpty.
]

{ #category : 'tests' }
RwUnpackagedBrowserApiTest >> testIssue420_C [

	"https://github.com/GemTalk/Rowan/issues/420"

	"move method from hypbrid package category to unpackaged class package category"

	| projectName  packageName1 packageName2 projectDefinition projectSetDefinition audit testClass |

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

"move"
	testClass addCategory: 'accessing'.
	[ testClass rwMoveMethod: #bar toCategory:  'accessing' ]
		on: RwPerformingUnpackagedEditNotification
		do: [:ex | ex resume ].	

"validate"
	self assert: (testClass compiledMethodAt: 'bar' otherwise: nil) notNil.
	self assert: (testClass categoryOfSelector: #bar) = 'accessing'  asSymbol.

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
RwUnpackagedBrowserApiTest >> testIssue420_D [

	"https://github.com/GemTalk/Rowan/issues/420"

	"move method from class category to packaged extension category in an unpackaged class"

	| projectName  packageName1 projectDefinition projectSetDefinition audit testClass |

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

"move"
	testClass addCategory: '*' , packageName1 asLowercase.
	[ testClass rwMoveMethod: #bar toCategory: '*' , packageName1 asLowercase ]
			on: RwPerformingUnpackagedEditNotification
			do: [:ex | ex resume ].	

"validate"
	self assert: (testClass compiledMethodAt: 'bar' otherwise: nil) notNil.
	self assert: (testClass categoryOfSelector: #bar) = ('*' , packageName1 asLowercase) asSymbol.

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
]
