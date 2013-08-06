"
notice: This work is protected by copyright. All rights reserved.
category: initialization
"
className: aName classIsMeta: isMetaclass selector: aSelector category: aCategory source: aSource

	className := aName.
	classIsMeta := isMetaclass.
	selector := aSelector.
	category := aCategory.
	source := self normalizeLineEndings: aSource