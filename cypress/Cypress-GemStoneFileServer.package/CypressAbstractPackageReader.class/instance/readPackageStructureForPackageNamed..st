"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: reading
"
readPackageStructureForPackageNamed: packageName

	| structureName |
	structureName := packageName , self repository packageExtension.
	self
		packageStructure: (CypressPackageStructure named: structureName);
		packageDirectory: (self fileUtils directoryFromPath: structureName
					relativeTo: self repository directoryPath);
		readPackageStructure
