private
materialize: string
  ^ (STONUtf8Reader on: string decodeFromUTF8 readStream) next