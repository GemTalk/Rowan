audit
errorLog: aResult add: aMessage	
"add error to results. print to file"
	aResult add: aMessage.
	GsFile gciLogServer: aMessage value asString,'  ', aMessage key asString.
	Notification signal: aMessage value asString,'  ', aMessage key asString.
