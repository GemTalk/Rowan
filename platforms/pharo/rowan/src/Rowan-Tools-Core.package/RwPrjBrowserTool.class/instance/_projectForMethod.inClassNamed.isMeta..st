private
_projectForMethod: methodSelector inClassNamed: className isMeta: isMeta

	^ (self
		_loadedProjectForMethod: methodSelector
		inClassNamed: className
		isMeta: isMeta) asDefinition