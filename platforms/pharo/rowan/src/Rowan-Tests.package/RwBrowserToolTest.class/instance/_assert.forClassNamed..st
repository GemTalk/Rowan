private
_assert: foundBlock forClassNamed: className

	"validate loaded class registry structure"

	self
		_assert: foundBlock
		forClassName: className
		ifAbsent: [ 
			self
				assert: false
				description:
					'expected class definitions for class ' , className printString , ' not found.' ]