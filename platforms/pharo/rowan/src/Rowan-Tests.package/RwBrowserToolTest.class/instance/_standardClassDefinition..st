private
_standardClassDefinition: className

	| classDefinition |
	classDefinition := self _simpleClassDefinition: className.

	classDefinition
		addInstanceMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'ivar1'
						protocol: 'accessing'
						source: 'ivar1 ^ivar1');
		addClassMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'initialize'
						protocol: 'initialization'
						source: 'initialize civar1 := 1. Cvar1 := 2.');
		addClassMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'civar1'
						protocol: 'accessing'
						source: 'civar1 ^civar1');
		addClassMethodDefinition:
				(RwMethodDefinition
						newForSelector: #'cvar1'
						protocol: 'accessing'
						source: 'cvar1 ^Cvar1');
		yourself.

	^ classDefinition