private
_specialCases
	| nonImplemented |
	nonImplemented := Set new.
	(self class allSelectors
		select: [ :ea | ea beginsWith: 'nonImplementedSelectors' ])
		do: [ :selector | nonImplemented addAll: (self perform: selector) ].
	^ nonImplemented