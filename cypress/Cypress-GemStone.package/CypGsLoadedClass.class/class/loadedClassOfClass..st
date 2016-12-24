"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: queries
"
loadedClassOfClass: aClass

	^ClassRegistry at: aClass classHistory ifAbsent: [nil]