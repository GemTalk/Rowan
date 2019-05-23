private
_basicClassDefinition: className superclass: superclass gs_options: gs_options type: type

	| def |
	def := RwClassDefinition
		newForClassNamed: className
		super: superclass
		instvars: #()
		classinstvars: #()
		classvars: #()
		category: ''
		comment: ''
		pools: #()
		type: type.
	gs_options ifNil: [ ^ def ].
	(gs_options _isArray and: [ gs_options isEmpty not ])
		ifTrue: [ 
			def properties
				at: 'gs_options'
				put:
					(gs_options collect: [ :each | each asString ]) asSortedCollection asArray ].
	^ def