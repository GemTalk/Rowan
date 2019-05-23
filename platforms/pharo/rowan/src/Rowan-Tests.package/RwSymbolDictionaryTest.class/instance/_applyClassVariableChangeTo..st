private
_applyClassVariableChangeTo: existingClass

	"benign properties are properties that do not cause a new class version or modify class variables"

	| newClass |
	newClass := existingClass superclass
		subclass: existingClass name
		instVarNames: #()
		classVars: self _classVars
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: nil
		newVersionOf: existingClass
		description: ''
		options: #().
	self assert: newClass == existingClass.	"not supposed to create a new class version"
	^ newClass