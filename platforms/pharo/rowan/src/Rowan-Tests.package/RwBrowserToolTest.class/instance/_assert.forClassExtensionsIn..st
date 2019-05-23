private
_assert: foundBlock forClassExtensionsIn: className

	"validate loaded class extension registry structure"

	self
		_assert: foundBlock
		forClassExtensionsIn: className
		ifAbsent: [ 
			self
				assert: false
				description:
					'expected class extension definitions for class ' , className printString
						, ' not found.' ]