"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: updating
"
updateKnownPackageRepository: aCypressRepository
	"Update the receiver to reflect it being a known package."

	self
		beKnown;
		updateRepository: aCypressRepository;
		refresh.
