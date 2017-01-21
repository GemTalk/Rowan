"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: reading
"
readDefinitionOfPackageNamed: packageName

	| fullPath reader |
	fullPath := self fullPathForPackageNamed: packageName.
	reader := self newPackageReader.
	^reader readPackageDefinitionFromDirectory: fullPath