"
notice: This work is protected by copyright. All rights reserved.
category: writing
"
writePackageStructure

	self
		writePackageStructureClasses: self packageStructure classes
			isClassExtension: false;
		writePackageStructureClasses: self packageStructure extensions
			isClassExtension: true
