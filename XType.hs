module XType where

import XLexer
import XUtils
import Text.Parsec

data XTypeSpecifier    =  Void | Char | UChar | Short | UShort | Int | UInt |
                          Chan | ChanEnd | Port | Timer | Clock |
                          Struct [(XType, String)] | Union [(XType, String)]
data XTypeQualifier    =  Const | Volatile | In | Out | Buffered
data XTypeStorageClass =  Auto | Register | Static | Extern | Inline
data XType = XType [XTypeStorageClass] [XTypeQualifier] XTypeSpecifier
