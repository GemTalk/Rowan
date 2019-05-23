visiting
visitExtensionMethodsModification: aMethodExtensionsModification

	aMethodExtensionsModification elementsModified
		do: [ :each | each acceptVisitor: self ]