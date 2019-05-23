private-issue 24
_removePackageNamed: packageName from: projectName validate: validate

	"validate the dirty state of project before and after remove package call"

	| project |

	project := RwProject newNamed: projectName.

	validate ifTrue: [ self deny: project isDirty ].

	Rowan projectTools browser removePackageNamed: packageName.

	(Rowan image loadedPackageNamed: packageName ifAbsent: [])
		ifNotNil: [:loadedPackage | self assert: false description: 'unexpected loaded package ', packageName printString ].

	validate ifTrue: [ self assert: project isDirty ].
