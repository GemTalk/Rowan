visiting
visitPackagesModification: aPackagesModification

	aPackagesModification elementsModified do: [ :each | each acceptVisitor: self ]