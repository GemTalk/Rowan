"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: installing
"
setProtocolTo: protocolString

	| protocolSymbol |
	protocolSymbol := protocolString asSymbol.
	(behavior includesCategory: protocolSymbol)
		ifFalse: [behavior addCategory: protocolSymbol].
	behavior moveMethod: selector toCategory: protocolSymbol