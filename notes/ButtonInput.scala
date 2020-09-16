val in2 = GPIO.DigitalIn(RPi.Pin(2), pull = Some(true))
val ts = TimeStamp()
val pressed   = (!in2).toTrig
val released  = in2.toTrig
val tPressed  = Var(0L)
val dPressed  = ts - tPressed
val cntPress  = Var(0)

val dlyPress  = Delay(4.0)

pressed ---> Act(
  ts.update,
  dlyPress,
  If (dPressed < 1500L) Then Act(
    cntPress.set(cntPress + 1),
    PrintLn("Click count: " ++ cntPress.toStr),
  ) Else {
    cntPress.set(1)
  },
  tPressed.set(ts), 
)

released ---> dlyPress.cancel

dlyPress ---> Act(
  PrintLn("Pressed for at least 4 seconds.")
)

