module Import(module Import) where
{
    -- base
    import Prelude as Import (Integer,undefined,error,Integral(..),fromIntegral,Ord(..),Enum(..),Num(..),IO,Fractional(..),(^^));
    import Data.Type.Equality as Import ((:~:)(..),TestEquality(..));
    import Data.Function as Import;
    import Data.Functor as Import;
    import Data.Foldable as Import;
    import Data.Traversable as Import;
    import Data.Bool as Import;
    import Data.Eq as Import;
    import Data.Maybe as Import;
    import Data.Either as Import;
    import Data.Tuple as Import;
    import Data.List as Import ((++),take,drop,intercalate,filter);
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
    import Foreign.C.Types as Import;
    
    -- transformers
    import Data.Functor.Identity as Import;
    import Data.Functor.Compose as Import;
    import Control.Monad.Trans.Class as Import;
    import Control.Monad.Trans.State as Import hiding (get);
    
    -- witness
    import Data.Witness.List as Import;
    
    -- bytestring
    import Data.ByteString as Import (ByteString);
}
