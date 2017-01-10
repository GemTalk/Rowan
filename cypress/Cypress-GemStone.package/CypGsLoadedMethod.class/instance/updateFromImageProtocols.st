"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: updating
"
updateFromImageProtocols
	"My information about a method may be out of date. Update it.
	Things that might have caused me to need updating:
	* There may no longer be a method with my name (selector) defined in the class.
	* There may be a different method defined with my name.
	* The method may be in a different protocol.
	* If the protocol has changed, the method may be in a new package."

	self handleMethodDeletionOrReplacement
		ifTrue: [self updateForProtocolChange]
		ifFalse: 
			["Method has been deleted and the deletion handled."
			]