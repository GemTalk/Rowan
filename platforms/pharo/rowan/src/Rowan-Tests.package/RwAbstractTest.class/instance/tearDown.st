running
tearDown

	super tearDown.
	Rowan platform _alternateImageClass: nil.
	self class cleanup.
