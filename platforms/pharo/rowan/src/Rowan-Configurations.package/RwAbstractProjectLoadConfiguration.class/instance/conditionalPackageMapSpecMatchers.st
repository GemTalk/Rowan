private
conditionalPackageMapSpecMatchers

	conditionalPackageMapSpecMatchers ifNotNil: [:val | ^ val ]. 
	conditionalPackageMapSpecMatchers := Dictionary new.
	self conditionalPackageMapSpecs keysAndValuesDo: [:platformPattern :packageMapSpecsMap |
		conditionalPackageMapSpecMatchers
			at: (self _platformPatternMatcherFor: platformPattern)
			put: packageMapSpecsMap ].
	^ conditionalPackageMapSpecMatchers
