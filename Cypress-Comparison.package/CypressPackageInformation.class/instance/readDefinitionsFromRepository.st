"
notice: This work is protected by copyright. All rights reserved.
category: updating
"
readDefinitionsFromRepository

	^(self repository reader readPackageStructureForPackageNamed: self name)
		packageStructure snapshot
		definitions