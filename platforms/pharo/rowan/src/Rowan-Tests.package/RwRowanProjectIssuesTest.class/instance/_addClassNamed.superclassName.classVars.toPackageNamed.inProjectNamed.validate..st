private
_addClassNamed: className superclassName: superclassName classVars: classVars toPackageNamed: packageName inProjectNamed: projectName validate: validate

	"validate the dirty state of project before and after class is added"

	| project package theClass |

	project := RwProject newNamed: projectName.
	package := RwPackage newNamed: packageName.

	validate ifTrue: [ 
		self deny: project isDirty.
		self deny: package isDirty ].

	theClass := (Rowan globalNamed: superclassName)
		rwSubclass: className
		instVarNames: #()
		classVars: classVars
		classInstVars: #()
		poolDictionaries: #()
		category: packageName
		options: #().

	validate ifTrue: [ 
		self assert: project isDirty.
		self assert: package isDirty ].

	^theClass
