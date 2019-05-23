private
_loadedMethod: methodSelector inClassNamed: className isMeta: isMeta

	^ self
		_loadedMethod: methodSelector
		inClassNamed: className
		isMeta: isMeta
		ifAbsent: [ 
			self
				error:
					'No loaded method for ' , methodSelector printString , ' was found for class '
						, className printString ]