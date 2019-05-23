private
_assertNoClassExtensionsIn: className

	"validate loaded class extension registry structure"

	self
		_assert: [ :classExtensionDef :packageDef :projectDef | 
			self
				assert: false
				description:
					'No class extensions expected for class ' , className printString ]
		forClassExtensionsIn: className
		ifAbsent: [  ]