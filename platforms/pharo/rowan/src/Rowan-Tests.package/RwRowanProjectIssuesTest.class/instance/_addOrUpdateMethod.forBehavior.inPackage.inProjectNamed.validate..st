private
_addOrUpdateMethod: methodSource forBehavior: theBehavior inPackage: packageName inProjectNamed: projectName validate: validate

	"validate the dirty state of project before and after method is added or updated"

	| project package |

	project := RwProject newNamed: projectName.
	package := RwPackage newNamed: packageName.

	validate ifTrue: [ 
		self deny: project isDirty.
		self deny: package isDirty].

	theBehavior rowanPackageName = packageName
		ifTrue: [
			theBehavior
				rwCompileMethod: methodSource
				category:  'other' ]
		ifFalse: [
			theBehavior
				rwCompileMethod: methodSource
				category:  '*' , packageName asLowercase ].

	validate ifTrue: [ 
		self assert: project isDirty.
		self assert: package isDirty ].
