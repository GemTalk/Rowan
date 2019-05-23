private-issue 24
_removeClassNamed: className fromProjectNamed: projectName validate: validate

	"validate the dirty state of project before and after class is added"

	| project package |

	project := RwProject newNamed: projectName.
	package := RwPackage newNamed: (Rowan globalNamed: className) rowanPackageName.

	validate ifTrue: [ 
		self deny: project isDirty.
		self deny: package isDirty ].

	Rowan projectTools browser removeClassNamed: className.

	validate ifTrue: [ 
		self assert: project isDirty.
		self assert: package isDirty ].
