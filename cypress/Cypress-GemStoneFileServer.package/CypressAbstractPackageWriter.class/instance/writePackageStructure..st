"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: writing
"
writePackageStructure: aPackageStructure

	self
		packageStructure: aPackageStructure;
		packageDirectory: self determinePackageDirectory;
		removeOldReplacingWithNew
