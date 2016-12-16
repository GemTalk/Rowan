"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: queries
"
loadedPackageOfClass: aClass

	^PackagesForClasses at: aClass classHistory ifAbsent: [nil]