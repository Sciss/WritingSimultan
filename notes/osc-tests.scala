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


// table 1
t.send(osc.Message("/shutdown"), "192.168.0.22" -> 57120)
t.send(osc.Message("/shutdown"), "192.168.0.24" -> 57120)
t.send(osc.Message("/shutdown"), "192.168.0.28" -> 57120)


// table 1
t.send(osc.Message("/next", 0), "192.168.0.22" -> 57120)
t.send(osc.Message("/next", 1), "192.168.0.24" -> 57120)
t.send(osc.Message("/next", 2), "192.168.0.28" -> 57120)

// table 2
t.send(osc.Message("/next", 0), "192.168.0.13" -> 57120)
t.send(osc.Message("/next", 1), "192.168.0.25" -> 57120)
t.send(osc.Message("/next", 2), "192.168.0.27" -> 57120)

// table 3
t.send(osc.Message("/next", 0), "192.168.0.23" -> 57120)
t.send(osc.Message("/next", 1), "192.168.0.47" -> 57120)
t.send(osc.Message("/next", 2), "192.168.0.48" -> 57120)


t.send(osc.Message("/next", 0), "192.168.0.77" -> 57120)
t.send(osc.Message("/mute", 1), "192.168.0.77" -> 57120)
t.send(osc.Message("/mute", 0), "192.168.0.77" -> 57120)
t.send(osc.Message("/volume", 0.25f), "192.168.0.77" -> 57120)
t.send(osc.Message("/volume", 1.0f), "192.168.0.77" -> 57120)

// table 1
t.send(osc.Message("/volume", 1f), "192.168.0.22" -> 57120)
t.send(osc.Message("/volume", 1f), "192.168.0.24" -> 57120)
t.send(osc.Message("/volume", 1f), "192.168.0.28" -> 57120)

// table 2
t.send(osc.Message("/volume", 0.25f), "192.168.0.13" -> 57120)
t.send(osc.Message("/volume", 0.25f), "192.168.0.25" -> 57120)
t.send(osc.Message("/volume", 0.25f), "192.168.0.27" -> 57120)

// table 3
t.send(osc.Message("/volume", 0.25f), "192.168.0.23" -> 57120)
t.send(osc.Message("/volume", 0.25f), "192.168.0.47" -> 57120)
t.send(osc.Message("/volume", 0.25f), "192.168.0.48" -> 57120)


// table 1
t.send(osc.Message("/mute", 1), "192.168.0.22" -> 57120)
t.send(osc.Message("/mute", 1), "192.168.0.24" -> 57120)
t.send(osc.Message("/mute", 1), "192.168.0.28" -> 57120)

