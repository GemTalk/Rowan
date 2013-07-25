writing
writePackageStructure

	self
		writePackageStructureClasses: self packageStructure classes
			isClassExtension: false;
		writePackageStructureClasses: self packageStructure extensions
			isClassExtension: true