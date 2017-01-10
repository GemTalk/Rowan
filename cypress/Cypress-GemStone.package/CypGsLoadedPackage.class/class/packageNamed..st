"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: queries
"
packageNamed: aName

	^PackageRegistry at: aName ifAbsent: [nil]