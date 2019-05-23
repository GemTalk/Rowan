private
_applyBenignPropertiesTo: existingClass

	"benign properties are properties that do not cause a new class version or modify class variables"

	| newClass |
	newClass := existingClass superclass
		subclass: existingClass name
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: nil
		newVersionOf: existingClass
		description: self _classComment
		options: #().
	self assert: newClass == existingClass.	"not supposed to create a new class version"
	^ newClass