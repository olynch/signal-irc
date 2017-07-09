module SignalAPI
  (
  getContactName,
  getGroupMembers,
  getGroupName,
  sendEndSessionMessage,
  sendGroupMessage,
  sendMessage,
  setContactName,
  updateGroup,
  DBus,
  DBusError,
  runDBus
  )
where

import Protolude hiding (Type)
import DBus.Internal.Message
import DBus.Internal.Types
import DBus.Client hiding (throwError)

data DBusError = ConversionError Type Type | ArityError Int Int | CommunicationError MethodError
  deriving (Show, Eq)

newtype DBus a = DBus { unDBus :: ReaderT Client (ExceptT DBusError IO) a }
  deriving (Functor, Applicative, Monad, MonadReader Client, MonadError DBusError, MonadIO)

runDBus :: Client -> DBus a -> IO (Either DBusError a)
runDBus client dbus = runExceptT $ runReaderT (unDBus dbus) client

signalMCBase :: MethodCall
signalMCBase = MethodCall {
    methodCallPath = "/org/asamk/Signal"
  , methodCallInterface = Just "org.asamk.Signal"
  , methodCallMember = ""
  , methodCallSender = Nothing
  , methodCallDestination = Just "org.asamk.Signal"
  , methodCallReplyExpected = True
  , methodCallAutoStart = False
  , methodCallBody = [] }

queryDBus :: MethodCall -> DBus [Variant]
queryDBus mc = do
  client <- ask
  result <- liftIO $ call client mc
  case result of
    Right ret -> return $ methodReturnBody ret
    Left err -> throwError $ CommunicationError err

querySignal :: MemberName -> [Variant] -> DBus [Variant]
querySignal member body = queryDBus signalMCBase { methodCallMember = member, methodCallBody = body }

convertDBus :: (IsVariant a) => Type -> [Variant] -> DBus a
convertDBus ty vars = case vars of
    [var] -> maybe (throwError $ ConversionError ty (typeOf var)) return (fromVariant var)
    _ -> throwError $ ArityError (length vars) 1

assertEmpty :: [Variant] -> DBus ()
assertEmpty vars = case vars of
    [] -> return ()
    _ -> throwError $ ArityError (length vars) 0

getContactName :: Text -> DBus Text
getContactName number = querySignal "getContactName" [ toVariant number ] >>= convertDBus TypeString

getGroupMembers :: ByteString -> DBus [Text]
getGroupMembers id = querySignal "getGroupMembers" [ toVariant id ] >>= convertDBus (TypeArray TypeString)

getGroupName :: ByteString -> DBus Text
getGroupName id = querySignal "getGroupName" [ toVariant id ] >>= convertDBus TypeString

-- not exactly sure what the args are for
sendEndSessionMessage :: [Text] -> DBus ()
sendEndSessionMessage msgs = querySignal "sendEndSessionMessage" [toVariant msgs] >>= assertEmpty

-- not exactly sure what the args are for
sendGroupMessage :: Text -> [Text] -> ByteString -> DBus ()
sendGroupMessage arg0 arg1 arg2 = querySignal "sendGroupMessage" [toVariant arg0, toVariant arg1, toVariant arg2]
  >>= assertEmpty

-- not sure what the difference between the two different forms are, just going to do the second one
-- don't know what the middle argument is
sendMessage :: Text -> [Text] -> Text -> DBus ()
sendMessage msg unk recipient = querySignal "sendMessage" [toVariant msg, toVariant unk, toVariant recipient]
  >>= assertEmpty

-- not sure which argument is which
setContactName :: Text -> Text -> DBus ()
setContactName arg0 arg1 = querySignal "setContactName" [toVariant arg0, toVariant arg1] >>= assertEmpty

-- not sure what the arguments are
updateGroup :: ByteString -> Text -> [Text] -> Text -> DBus ()
updateGroup arg0 arg1 arg2 arg3 = querySignal "updateGroup"
  [toVariant arg0, toVariant arg1, toVariant arg2, toVariant arg3]
  >>= assertEmpty
