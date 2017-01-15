"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: queries
"
existingLoadedClassForClass: aClass
	"If there is an existing loadedClass for the given (non-meta) class in this package, answer it, otherwise the AbsentToken."

	^loadedClasses at: aClass name asString ifAbsent: [AbsentToken]