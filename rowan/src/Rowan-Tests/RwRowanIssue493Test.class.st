Class {
	#name : 'RwRowanIssue493Test',
	#superclass : 'RwRowanProjectIssuesTest',
	#category : 'Rowan-Tests'
}

{ #category : 'tests' }
RwRowanIssue493Test >> testMoveClassBetweenSymDicts_changeDefaulSymDict_1 [

	"https://github.com/dalehenrich/Rowan/issues/493"

	"move a class with no methods from one sym dict to another using defaultSymbolDictName"

	| className packageName projectName projectDefinition project audit |

	projectName := 'Issue493'.
	packageName := 'Issue493-Core'.
	className := 'Issue493Class'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	projectDefinition := (RwComponentProjectDefinition projectName: projectName)
		defaultSymbolDictName: self _symbolDictionaryName1;
		addPackageNamed: packageName;
		yourself.

	((projectDefinition packageNamed: packageName)
		addClassNamed: className super: 'Object' category: '*', packageName asLowercase).

"load"
	projectDefinition load.

"validate"
	project := Rowan projectNamed: projectName.
	self assert: (audit := project audit isEmpty).

"switch default symbol dict"
	projectDefinition defaultSymbolDictName: self _symbolDictionaryName2.

"load"
	projectDefinition load.

"validate"
	self assert: (audit := project audit) isEmpty.
]

{ #category : 'tests' }
RwRowanIssue493Test >> testMoveClassBetweenSymDicts_changeDefaulSymDict_2 [

	"https://github.com/dalehenrich/Rowan/issues/493"

	"move a class with a method from one sym dict to another using defaultSymbolDictName"

	| className packageName projectName projectDefinition project audit |

	projectName := 'Issue493'.
	packageName := 'Issue493-Core'.
	className := 'Issue493Class'.
	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	projectDefinition := (RwComponentProjectDefinition projectName: projectName)
		defaultSymbolDictName: self _symbolDictionaryName1;
		addPackageNamed: packageName;
		yourself.

	((projectDefinition packageNamed: packageName)
		addClassNamed: className super: 'Object' category: '*', packageName asLowercase)
			addInstanceMethod: 'foo ^ 1' protocol: 'accessing'.

"load"
	projectDefinition load.

"validate"
	project := Rowan projectNamed: projectName.
	self assert: (audit := project audit isEmpty).

"switch default symbol dict"
	projectDefinition defaultSymbolDictName: self _symbolDictionaryName2.

"load"
	projectDefinition load.

"validate"
	self assert: (audit := project audit) isEmpty.
]
