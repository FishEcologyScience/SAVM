# alert message helpers works

    Code
      sav_msg_info("info")
    Message
      i info

---

    Code
      sav_msg_success("success")
    Message
      v success

---

    Code
      sav_msg_warning("warning")
    Message
      ! warning

---

    Code
      sav_msg_danger("danger")
    Message
      x danger

---

    Code
      withr::with_options(list(savm.verbose = "q"), sav_msg_info("info"))
    Output
      NULL

# message helpers should work

    Code
      sav_inform("info")
    Message
      info

---

    Code
      sav_debug_msg("debug")

---

    Code
      withr::with_options(list(savm.verbose = "debug"), sav_debug_msg("debug"))
    Message
      debug

