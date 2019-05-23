audit
_auditCategory: category forBehavior: aBehavior loadedClass: aLoadedClass
	| aPackage |
		"must be an extension. Do basic checks"
	aPackage := (category copyFrom: 2 to: category size).

	^category first == $*	ifTrue: ["must be an extension category. See if package exists"
		(aPackage asLowercase isEquivalent: aBehavior rowanPackageName asLowercase) 
			ifTrue: [{aLoadedClass name , ' #' , category asString -> 'Extension category name can not be same as class package' }]
			ifFalse: [
				(Rowan image loadedClassExtensionsNamed: aBehavior thisClass name 
					ifFound: [:extensions | extensions detect: [:e | e loadedPackage name asLowercase isEquivalent: aPackage] ifNone: [nil]] 
					ifAbsent: [nil]) isNil 
						ifTrue: [ | res | 
									res := self _auditCategory: category selectors: (aBehavior selectorsIn: category)  forBehavior: aBehavior loadedClass: aLoadedClass		.
									"res add: (aLoadedClass name , ' #' , category asString -> 'Class Extension is not present in the package '); 
									yourself"
						] 
						ifFalse: [{}"no basic extension problems found, class extension will be audited separately"]
			]
	] ifFalse: [
		self _auditCategory: category selectors: (aBehavior selectorsIn: category)  forBehavior: aBehavior loadedClass: aLoadedClass		
	]
