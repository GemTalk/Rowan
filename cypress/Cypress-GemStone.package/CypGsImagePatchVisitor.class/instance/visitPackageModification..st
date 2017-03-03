"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: visting
"
visitPackageModification: aPackageModification

	| propertiesModification classesModification classExtensionsModification |
	currentPackage := aPackageModification after.
	propertiesModification := aPackageModification propertiesModification.
	propertiesModification isEmpty
		ifFalse: 
			[CypGsPackagePropertiesPatch forPackageProperties: propertiesModification
				inPackage: currentPackage].
	aPackageModification classesModification acceptVisitor: self.
	aPackageModification classExtensionsModificationModification
		acceptVisitor: self