instance creation
newForSource: methodSource protocol: protocol

	| selector |
	selector := Rowan platform parseSelectorFrom: methodSource.
	^ self
		newForSelector: selector asSymbol
		protocol: protocol
		source: methodSource