"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: queries
"
forClass: aClass
	"Answer the registered LoadedClass for the given class. If none registered, create one, register it, and answer it."

	^ClassRegistry at: aClass classHistory ifAbsent: [self newForClass: aClass]