module internal Dict

    type Dict<'a when 'a : comparison>
    val empty : unit -> Dict<'a>
    val insert : string -> Dict<'a> -> Dict<'a>
    val lookup : string -> Dict<'a> -> bool
    val step : char -> Dict<'a> -> (bool*Dict<'a>) option