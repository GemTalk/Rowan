private
conditionalPackageMatchers

	conditionalPackageMatchers ifNotNil: [:val | ^ val ]. 
	conditionalPackageMatchers := Dictionary new.
	self conditionalPackages keysAndValuesDo: [:platformPatterns :groupMap |
		conditionalPackageMatchers
			at: (platformPatterns collect: [:pattern | self _platformPatternMatcherFor: pattern ])
			put: groupMap ].
	^ conditionalPackageMatchers
