"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: initializing - private
"
initializeMessageText

	messageText := String streamContents: 
					[:stream |
					stream nextPutAll: 'Missing classes:'.
					self requirementsMap keysAndValuesDo: 
							[:className :definitions |
							stream
								space;
								nextPutAll: className printString , '(' , definitions size printString
											, ')']]