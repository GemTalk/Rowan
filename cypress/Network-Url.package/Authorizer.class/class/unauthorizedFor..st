"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: as yet unclassified
"
unauthorizedFor: realm
  ^ 'HTTP/1.0 401 Unauthorized' , self crlf
    , 'WWW-Authenticate: Basic realm="Pharo/' , realm , '"'
    ,
      (String
        with: Character cr
        with: Character lf
        with: Character cr
        with: Character lf)
    , '<html><title>Unauthorized</title><body><h2>Unauthorized for ' , realm
    , '</h2></body></html>'
