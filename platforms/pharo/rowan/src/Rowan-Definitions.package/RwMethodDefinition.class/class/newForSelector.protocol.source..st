instance creation
newForSelector: selector protocol: protocol source: source

	^ self
		withProperties:
			(Dictionary with: 'selector' -> selector with: 'protocol' -> protocol)
		source: source