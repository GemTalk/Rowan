accessing
approximateBase

	| base condensed |
	condensed := self collapseZeros.
	base := condensed copyFrom: 1 to: condensed size - 1.
	base at: base size put: (base at: base size) + 1.
	^base
