private
_simpleClassDefinition: className

	^ RwClassDefinition
		newForClassNamed: className
		super: 'Object'
		instvars: #('ivar1')
		classinstvars: #('civar1')
		classvars: #('Cvar1')
		category: 'Simple Things'
		comment: 'I am a SimpleEdit class'
		pools: #()
		type: 'normal'