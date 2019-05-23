private
_loadedProjectForMethod: methodSelector inClassNamed: className isMeta: isMeta

	^ (self _loadedMethod: methodSelector inClassNamed: className isMeta: isMeta)
		loadedProject