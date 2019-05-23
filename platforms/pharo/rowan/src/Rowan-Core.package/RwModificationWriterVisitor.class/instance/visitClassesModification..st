visiting
visitClassesModification: aClassesModification

	aClassesModification elementsModified
		do: [:each | each acceptVisitor: self]