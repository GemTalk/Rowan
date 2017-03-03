"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: visting
"
visitClassesModification: aClassesModification

	self
		addClasses: aClassesModification elementsAdded;
		deleteClasses: aClassesModification elementsRemoved.
	aClassesModification elementsModified
		do: [:each | each acceptVisitor: self]