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
	oldProtocol = newProtocol
		ifFalse: 
			[| newProtocolIsExtension |
			newProtocolIsExtension := newProtocol first = $*.
			oldProtocol first = $* | newProtocolIsExtension
				ifTrue: 
					[self
						updateForPackageChangeFrom: (oldProtocol copyFrom: 2 to: oldProtocol size)
						to: (newProtocol copyFrom: 2 to: newProtocol size)
						newIsExtension: newProtocolIsExtension].
			self
				markPackageDirty;
				propertyAt: 'protocol' put: newProtocol]