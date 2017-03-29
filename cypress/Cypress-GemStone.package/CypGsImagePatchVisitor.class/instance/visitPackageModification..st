"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: visting
"
visitPackageModification: aPackageModification

	| propertiesModification classesModification classExtensionsModification |
	aPackageModification isAddition
		ifTrue: 
			[self addAddedPackage: aPackageModification after.
			^self].
	aPackageModification isDeletion
		ifTrue: 
			[self addDeletedPackage: aPackageModification before.
			^self].
	currentPackage := aPackageModification after.
	propertiesModification := aPackageModification propertiesModification.
	propertiesModification isEmpty
		ifFalse: 
			[self halt: 'What do we do with this patch we''re creating here?'.
			CypGsPackagePropertiesPatch forPackageProperties: propertiesModification
				inPackage: currentPackage].
	aPackageModification classesModification acceptVisitor: self.
	aPackageModification classExtensionsModification acceptVisitor: self