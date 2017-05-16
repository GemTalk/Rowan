"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: installing
"
setProtocol
	"If I'm an extension method, my protocol in the image may need to be *<packageName>*."

	self
		setProtocolTo: (Cypress configuration needsPackageProtocolNames
				& self isForExtensionMethod
					ifTrue: [self packageProtocolName]
					ifFalse: [self propertiesProtocolName])