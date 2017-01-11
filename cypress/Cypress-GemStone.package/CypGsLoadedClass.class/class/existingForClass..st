"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: queries
"
existingForClass: aClass
	"Answer the registered LoadedClass for the given class."

	^ClassRegistry at: aClass classHistory ifAbsent: [AbsentToken]