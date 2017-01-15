"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private-updating
"
updateForProtocolChange
	"Check to see whether I'm in a different protocol. If I am, that could change which package I'm in and I need to update that."

	| oldProtocol newProtocol |
	oldProtocol := self propertyAt: 'protocol'.
	newProtocol := (handle inClass categoryOfSelector: handle selector)
				asString.

	"Must test for AbsentToken by identity, or a new protocol of '<absent>' would not  be detected."
	oldProtocol == AbsentToken | (oldProtocol ~= newProtocol)
		ifTrue: [self updateForProtocolChangeTo: newProtocol]