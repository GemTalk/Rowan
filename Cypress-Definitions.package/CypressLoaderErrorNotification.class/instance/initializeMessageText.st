initializing - private
initializeMessageText

	messageText := String streamContents: 
					[:stream |
					stream
						nextPutAll: self patchOperation printString;
						nextPutAll: ' failed because ';
						nextPutAll: self exception printString]