module AST where

type Definitions a = [(ExtendedAttributeList a, Definition a)]

data Definition a = CallbackOrInterface a
                  | Partial a
                  | Dictionary a
                  | Exception a
                  | Enum a
                  | Typedef a
                  | ImplementsStatement a

data CallbackOrInterface a = CallbackRestOrInterface a
                           | Interface a

