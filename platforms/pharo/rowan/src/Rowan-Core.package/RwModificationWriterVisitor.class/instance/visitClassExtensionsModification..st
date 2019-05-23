visiting
visitClassExtensionsModification: aClassExtensionsModification

	aClassExtensionsModification elementsModified
		do: [ :each | each acceptVisitor: self ]