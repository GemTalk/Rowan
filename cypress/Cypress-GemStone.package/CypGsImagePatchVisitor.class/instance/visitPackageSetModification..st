"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: visting
"
visitPackageSetModification: aPackageSetModification

	patchSet
		addAddedPackages: aPackageSetModification elementsAdded;
		addDeletedPackages: aPackageSetModification elementsRemoved.
	aPackageSetModification elementsModified
		do: [:each | each acceptVisitor: self]