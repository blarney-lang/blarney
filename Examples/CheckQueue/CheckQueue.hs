import Blarney
import Blarney.Queue

-- Test some queue properties
data QueueCmd a =
  QueueCmd {
    cmdEnq :: a -> Action ()
  , cmdDeq :: Action ()
  }
  deriving (Generic, Interface)

cosim :: (Num a, Bits a) => Queue a -> Queue a -> Module (QueueCmd a)
cosim q1 q2 = do
  always do
    assert (nameBits "prop_notFull" $ q1.notFull .==. q2.notFull)
           "q1 and q2 notFull signal should be equal"
    assert (nameBits "prop_notEmpty" $ q1.notEmpty .==. q2.notEmpty)
           "q1 and q2 notEmtpy signal should be equal"
    assert (nameBits "prop_First" $
                     q1.canDeq .&. q2.canDeq .==>. q1.first === q2.first)
           "q1 and q2 first elements should be equal"
  return
    QueueCmd {
      cmdEnq = \x ->
        when (q1.notFull .&. q2.notFull) do
          enq q1 x
          enq q2 x
    , cmdDeq =
        when (q1.canDeq .&. q2.canDeq) do
          q1.deq
          q2.deq
    }

prop_queueEquiv :: KnownNat n => Module (QueueCmd (Bit n))
prop_queueEquiv = do
  q <- makeQueue
  sq <- makeShiftQueue 2
  cosim q sq

-- Main function
main :: IO ()
main = writeSMTScript vCnf (prop_queueEquiv @1)
                      "prop_queueEquiv" "CheckQueue-SMT"
  where vCnf = dfltVerifyConf { verifyConfMode = Induction (fixedDepth 8) True }
