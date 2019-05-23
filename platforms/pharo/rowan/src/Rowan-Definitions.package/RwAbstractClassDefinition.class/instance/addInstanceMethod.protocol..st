accessing
addInstanceMethod: methodSource protocol: protocol

	^self addInstanceMethodDefinition: (RwMethodDefinition newForSource: methodSource protocol: protocol)