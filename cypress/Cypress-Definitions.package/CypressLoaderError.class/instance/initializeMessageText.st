"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: initializing - private
"
initializeMessageText

	messageText := String streamContents: 
					[:stream |
					stream
						nextPutAll: self patchOperation printString;
						nextPutAll: ' failed because ';
						nextPutAll: self exception printString]