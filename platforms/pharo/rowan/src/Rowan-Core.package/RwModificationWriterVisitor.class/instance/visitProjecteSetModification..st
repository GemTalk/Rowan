visiting
visitProjecteSetModification: aProjectSetModification

	aProjectSetModification elementsModified
		do: [ :each | each acceptVisitor: self ]