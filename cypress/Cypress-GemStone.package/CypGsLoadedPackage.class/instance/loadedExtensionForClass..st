"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: queries
"
loadedExtensionForClass: aClass
	"Answer an extension in this package for the given (non-meta) class. Create it if necessary."

	| loadedExtension |
	loadedExtension := self existingLoadedExtensionForClass: aClass.
	loadedExtension == AbsentToken
		ifTrue: 
			["Must create"
			(self existingLoadedClassForClass: aClass) == AbsentToken
				ifFalse: 
					[self
						error: 'Attempt to create a class extension in package that defines the class.'].
			loadedExtension := CypGsLoadedClassExtension newForClass: aClass
						inPackage: self].
	^loadedExtension