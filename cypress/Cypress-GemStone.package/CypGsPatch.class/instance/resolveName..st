"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: utility
"
resolveName: aName
	"Answer a SymbolAssociation for the given name."

	^GsCurrentSession currentSession resolveSymbol: aName asSymbol