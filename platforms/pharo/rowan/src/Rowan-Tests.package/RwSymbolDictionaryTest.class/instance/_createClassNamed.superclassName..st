private
_createClassNamed: className superclassName: superclassName

	| superclass |
	superclass := System myUserProfile objectNamed: superclassName.
	^ superclass
		subclass: className
		instVarNames: #()
		classVars: #()
		classInstVars: #()
		poolDictionaries: #()
		inDictionary: nil
		newVersionOf: nil
		description: ''
		options: #()