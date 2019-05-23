visiting
visitMethodsModification: aMethodsModification

	aMethodsModification elementsModified
		do: [:each | each acceptVisitor: self]