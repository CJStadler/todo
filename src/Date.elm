module Date exposing (Date, Serialized, deserialize, serialize)


type Date
    = Date Record


type alias Record =
    { year : Int
    , month : Int
    , dayOfMonth : Int
    }


type alias Serialized =
    Record


deserialize : Serialized -> Date
deserialize record =
    Date record


serialize : Date -> Serialized
serialize date =
    case date of
        Date r ->
            r
