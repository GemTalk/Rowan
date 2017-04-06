"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: queries
"
existingPackageNamed: aName ifAbsent: absentBlock

	^PackageRegistry at: aName ifAbsent: absentBlock