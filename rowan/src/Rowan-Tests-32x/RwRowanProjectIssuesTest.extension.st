Extension { #name : 'RwRowanProjectIssuesTest' }

{ #category : '*rowan-tests-32x' }
RwRowanProjectIssuesTest >> _createLoadedProjectNamed: projectName packageNames: packageNames root: rootPath symbolDictionaryName: symbolDictionaryName validate: validate [

	| projectDefinition project |

	(Rowan image loadedProjectNamed: projectName ifAbsent: [  ])
		ifNotNil: [ :prj | Rowan image _removeLoadedProject: prj ].

	projectDefinition := self
		_standardProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: symbolDictionaryName
		comment:
			'Basic project ', projectName printString.

	self
		handleConfirmationDuring: [
			Rowan projectTools create 
				createProjectFor: projectDefinition 
				format: 'tonel' 
				root:rootPath 
				configsPath: 'configs'
				repoPath: 'src' 
				specsPath: 'specs' ].

	Rowan projectTools load loadProjectDefinition: projectDefinition.

	project := RwProject newNamed: projectName.

	validate ifTrue: [ self assert: project isDirty ]. "a project is dirty if it has changes that are not written to disk"

]

{ #category : '*rowan-tests-32x' }
RwRowanProjectIssuesTest >> testIssue498_constraint_ordering_1 [

	"https://github.com/dalehenrich/Rowan/issues/498 -- expanded validation over https://github.com/dalehenrich/Rowan/issues/293"

	"constraints should be displayed in inst var order, not alphabetical order"

	"non-rowan variant of test ... _2 uses Rowan api"

	"creating constraints during G/S class creation is not supported in 3.4.x or later"

	| className1 className2 className3 class constraintBlock1 constraintBlock2 constraintBlock3 x
		class1 class2 class3 |
	className1 := 'Issue293Class1'.
	className2 := 'Issue293Subclass2'.
	className3 := 'Issue293Subclass3'.

	{className1 . className2. className3 }
		do: [ :className | 
			UserGlobals removeKey: className1 asSymbol ifAbsent: [].
			UserGlobals removeKey: className2 asSymbol  ifAbsent: [].
			UserGlobals removeKey: className3 asSymbol  ifAbsent: [] ].

	[ 
	class1 := Object
		subclass: className1
		instVarNames: #( ivar2 ivar3 ivar4 ivar1)
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: UserGlobals
		constraints: { { 'ivar4' . Association }. { 'ivar3' . Association }. { 'ivar2' . Association}. { 'ivar1' . Association }. }
		options: {}.

	class2 := class1
		subclass: className2
		instVarNames: #( ivar7 ivar6)
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: UserGlobals
		constraints: { { 'ivar7' . Association }. { 'ivar6' . Association }. }
		options: {}.

	class3 := class2
		subclass: className3
		instVarNames: #( ivar8 ivar9)
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: UserGlobals
		constraints: { { 'ivar9' . Association }. { 'ivar8' . Association }. }
		options: {} ] 
			on: Deprecated 
			do: [:ex | 
				"ignore any Deprecation errors ... we're using the deprecated class creation on purpose"
				ex resume ].

"validate"
	class := Rowan globalNamed: className1.
	self assert: (class _constraintsEqual: {  { #'ivar1' . Association }.  { #'ivar2' . Association }. { #'ivar3' . Association }. { #'ivar4' . Association }.  }).

	class := Rowan globalNamed: className2.
	self assert: (class _constraintsEqual: {  { #'ivar7' . Association }.  { #'ivar6' . Association }. }).

	class := Rowan globalNamed: className3.
	self assert: (class _constraintsEqual: {  { #'ivar8' . Association }.  { #'ivar9' . Association }. }).

	constraintBlock1 := [:theClass |
		self assert: (x := theClass _constraintOn: #ivar1) == Association.
		self assert: (x := theClass _constraintOn: #ivar2) == Association.
		self assert: (x := theClass _constraintOn: #ivar3) == Association.
		self assert: (x := theClass _constraintOn: #ivar4) == Association ].
	constraintBlock2 := [:theClass |
		self assert: (x := theClass _constraintOn: #ivar6) == Association.
		self assert: (x := theClass _constraintOn: #ivar7) == Association ].
	constraintBlock3 := [:theClass |
		self assert: (x := theClass _constraintOn: #ivar8) == Association.
		self assert: (x := theClass _constraintOn: #ivar9) == Association ].

	constraintBlock1 value: (Rowan globalNamed: className1).

	constraintBlock1 value: (Rowan globalNamed: className2).
	constraintBlock2 value: (Rowan globalNamed: className2).

	constraintBlock1 value: (Rowan globalNamed: className3).
	constraintBlock2 value: (Rowan globalNamed: className3).
	constraintBlock3 value: (Rowan globalNamed: className3).
]
