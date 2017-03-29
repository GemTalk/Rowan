"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: visting
"
visitPackageSetModification: aPackageSetModification

	aPackageSetModification elementsModified
		do: [:each | each acceptVisitor: self]