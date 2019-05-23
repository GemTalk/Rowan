audit
errorLog: aResult addAll: aCol	
"add all messages to result"
	aCol do: [:e | self errorLog: aResult add: e].