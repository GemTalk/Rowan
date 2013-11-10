"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: initialization
"
className: aName classIsMeta: isMetaclass selector: aSelector category: aCategory source: aSource

	className := aName.
	classIsMeta := isMetaclass.
	selector := aSelector.
	category := aCategory.
	source := self normalizeLineEndings: aSource