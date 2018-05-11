initialize-release
initialize
  "GemStone DateAndTime uses a float for seconds so serialize/materialize of floats is problematic ... this technique causes a ScaledDecimal to be used which does suffer from Float problems"

  created := modified := DateAndTime fromString: DateAndTime now asString