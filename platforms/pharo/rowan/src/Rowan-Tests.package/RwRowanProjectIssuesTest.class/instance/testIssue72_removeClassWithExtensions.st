tests-issue 72
testIssue72_removeClassWithExtensions
	"https://github.com/dalehenrich/Rowan/issues/72"

	"Need to worry about removing an unpackaged class that has extension methods"

	| className theClass projectName packageName packageNames |

	className := 'Issue72TestClass'.
	theClass := Object subclass: className
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: self _symbolDictionary
		options: #().
	self assert: (Rowan image objectNamed: className) notNil.
	self assert: (Rowan image loadedClassNamed: className ifAbsent: []) isNil.
	self assert: ((Rowan image loadedClassExtensionsForClass: theClass) select: [:each | each isEmpty not ]) isEmpty.

	projectName := 'Issue72Project'.
	packageName := 'Issue72Project-Extensions'.
	packageNames := {packageName}.
	self
		_loadProjectDefinition: projectName
		packageNames: packageNames
		defaultSymbolDictName: self _symbolDictionaryName
		comment: 'issue 72 project'.

	theClass rwCompileMethod: 'bar ^''bar'''
				category: '*', packageName asLowercase.

	self assert: (Rowan image objectNamed: className) notNil.
	self assert: (Rowan image loadedClassNamed: className ifAbsent: []) isNil.
	self assert: ((Rowan image loadedClassExtensionsForClass: theClass) select: [:each | each isEmpty not ]) notEmpty.

	[
		"handle notification and complete removal"
		Rowan projectTools browser removeClassNamed: className  ]
			on: RwPerformingUnpackagedEditNotification
			do: [:ex | ex resume ].
	self assert: (Rowan image objectNamed: className) isNil.
	self assert: (Rowan image loadedClassNamed: className ifAbsent: []) isNil.
	self assert: ((Rowan image loadedClassExtensionsForClass: theClass) select: [:each | each isEmpty not ]) isEmpty.
