"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: reading
"
readPackageStructureForPackageNamed: packageName

	^(self reader)
		readPackageStructureForPackageNamed: packageName;
		packageStructure
