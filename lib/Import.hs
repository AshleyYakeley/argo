module Import(module Import) where
{
    -- base
    import Prelude as Import (Int,Integer,undefined,error,Integral(..),fromIntegral,Ord(..),Enum(..),Num(..),IO,Fractional(..),(^^));
    import Data.Function as Import;
    import Data.Functor as Import;
    import Data.Foldable as Import;
    import Data.Traversable as Import;
    import Data.Bool as Import;
    import Data.Eq as Import;
    import Data.Maybe as Import;
    import Data.Either as Import;
    import Data.Tuple as Import;
    import Data.List as Import ((++),null,take,drop,intercalate);
    import Data.Word as Import;
    import Data.Int as Import;
    import Data.Char as Import;
    import Data.String as Import;
    import Control.Applicative as Import;
    import Control.Monad as Import (Monad(..),MonadPlus(..));
    import Control.Monad.Fix as Import;
    import Data.Ratio as Import;
    import Text.Show as Import (Show(..));
    import Text.Read as Import (Read(..),readListPrecDefault,readPrec_to_P,readP_to_Prec,readMaybe);
    import Text.ParserCombinators.ReadP as Import(ReadP,char,string,(<++),get,satisfy,skipSpaces,eof,readP_to_S);
    import Text.ParserCombinators.ReadPrec as Import(minPrec);
    import Foreign.C.Types as Import;
    
    -- transformers
    import Data.Functor.Identity as Import;
    import Data.Functor.Compose as Import;
    import Control.Monad.Trans.Class as Import;
    import Control.Monad.Trans.State as Import hiding (get);
    
    -- witness
    import Data.Witness.EqualType as Import;
    import Data.Witness.SimpleWitness as Import;
    import Data.Witness.List as Import;
    
    -- bytestring
    import Data.ByteString as Import (ByteString);

    optionalMax :: ReadP a -> ReadP (Maybe a);
    optionalMax p = (fmap Just p) <++ return Nothing;

    manyMax :: ReadP a -> ReadP [a];
    manyMax p =  many1Max p <++ return [];

    many1Max :: ReadP a -> ReadP [a];
    many1Max p = liftA2 (:) p (manyMax p);
}
