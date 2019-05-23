accessing
addClassMethod: methodSource protocol: protocol

	^self addClassMethodDefinition: (RwMethodDefinition newForSource: methodSource protocol: protocol)