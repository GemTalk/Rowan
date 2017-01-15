"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: queries
"
existingLoadedExtensionForClass: aClass
	"If there is an existing extension for the given (non-meta) class in this package, answer it, otherwise the AbsentToken."

	^loadedClassExtensions at: aClass classHistory ifAbsent: [AbsentToken]