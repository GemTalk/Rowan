private
_applyClassNewVersionChangesTo: existingClass

	"benign properties are properties that do not cause a new class version or modify class variables"

	| newClass superclass |
	superclass := System myUserProfile objectNamed: 'Association'.
	newClass := superclass
		subclass: existingClass name
		instVarNames: self _instVars
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: nil
		newVersionOf: existingClass
		description: ''
		options: #().
	self assert: newClass ~~ existingClass.	"supposed to create a new class version"
	^ newClass