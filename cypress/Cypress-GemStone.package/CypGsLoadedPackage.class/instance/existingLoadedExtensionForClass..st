"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: queries
"
existingLoadedExtensionForClass: aClass
	"If there is an existing extension for the given class in this package, answer it, otherwise the AbsentToken."

	| loadedClass |
	loadedClass := CypGsLoadedClass existingForClass: aClass.
	^loadedExtendedClasses at: loadedClass key ifAbsent: [AbsentToken]