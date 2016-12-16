"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
loadedPackageNamed: aName ifNone: noneBlock

	^loadedPackages at: aName ifAbsent: noneBlock