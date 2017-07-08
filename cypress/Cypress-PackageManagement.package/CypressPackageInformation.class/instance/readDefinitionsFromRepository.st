"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: updating
"
readDefinitionsFromRepository

	^(self repository reader readPackageStructureForPackageNamed: self name)
		packageStructure snapshot
		definitions
