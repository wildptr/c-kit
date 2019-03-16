type int_size =
  | Size_Char
  | Size_Short
  | Size_Int
  | Size_Long
  | Size_Long_Long
[@@deriving show { with_path = false }]

type float_size =
  | Size_Float
  | Size_Double
  | Size_Long_Double
[@@deriving show { with_path = false }]
