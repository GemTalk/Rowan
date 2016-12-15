"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: queries
"
loadedPackageOfClass: aClass
	"If the given class is in a package, answer that LoadedPackage. If not, answer nil."

	^self loadedPackageClass loadedPackageOfClass: aClass