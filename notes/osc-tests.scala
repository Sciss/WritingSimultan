val t = osc.UDP.Transmitter()
val tgt = "192.168.0.77" -> 57120
// val tgt = "192.168.0.22" -> 57120
// val tgt = "127.0.0.1" -> 57120
t.send(osc.Message("/next", 0), tgt)
t.send(osc.Message("/next", 1), tgt)
t.send(osc.Message("/next", 2), tgt)
t.send(osc.Message("/next", 3), tgt)
t.send(osc.Message("/next", 4), tgt)
t.send(osc.Message("/next", 5), tgt)
t.send(osc.Message("/next", 6), tgt)
t.send(osc.Message("/next", 7), tgt)
t.send(osc.Message("/next", 8), tgt)


t.send(osc.Message("/shutdown"), "192.168.0.22" -> 57120)
t.send(osc.Message("/shutdown"), "192.168.0.24" -> 57120)
t.send(osc.Message("/shutdown"), "192.168.0.28" -> 57120)

t.send(osc.Message("/next", 0), "192.168.0.22" -> 57120)
t.send(osc.Message("/next", 1), "192.168.0.24" -> 57120)
t.send(osc.Message("/next", 2), "192.168.0.28" -> 57120)

t.send(osc.Message("/next", 0), "192.168.0.13" -> 57120)
t.send(osc.Message("/next", 1), "192.168.0.25" -> 57120)
t.send(osc.Message("/next", 2), "192.168.0.27" -> 57120)

t.send(osc.Message("/next", 0), "192.168.0.23" -> 57120)
t.send(osc.Message("/next", 1), "192.168.0.47" -> 57120)
t.send(osc.Message("/next", 2), "192.168.0.48" -> 57120)
