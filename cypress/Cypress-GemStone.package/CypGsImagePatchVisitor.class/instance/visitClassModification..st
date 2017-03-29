"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: visting
"
visitClassModification: aClassModification

	| propertiesModification classesModification classExtensionsModification |
	aClassModification isAddition
		ifTrue: 
			[patchSet addAddedClass: aClassModification after inPackage: currentPackage.
			^self].
	aClassModification isDeletion
		ifTrue: 
			[patchSet addDeletedClass: aClassModification before
				inPackage: currentPackage.
			^self].
	currentClass := aClassModification after.
	propertiesModification := aClassModification propertiesModification.
	propertiesModification isEmpty
		ifFalse: [self halt: 'Need to do something with modified properties'].
	aClassModification instanceMethodsModification acceptVisitor: self.
	aClassModification classMethodsModification acceptVisitor: self