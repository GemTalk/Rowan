"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
expect: aString
	"Expects aString and consume input, throw an error otherwise."

	^(self match: aString)
		ifFalse: [CypressJsonError signal: aString , ' expected']
