
res <- NULL
for(x_1 in 0:1) {
  for(m_2 in 0:1) {

    raw <- readLines(file.path("bounds-latex", sprintf("cn1cde2.%s%s.txt", x_1, m_2)))
    lower = paste(raw[c((which(raw == "MAX {")+1):(which(raw == "}")[1]-1))], collapse = "\n")
    upper = paste(raw[c((which(raw == "MIN {")+1):(which(raw == "}")[2]-1))], collapse = "\n")
    res <- paste(res, "\n", glue("\\noindent \\textbf{Result x}:\\\\\\
    The bounds given below are valid and tight for
    $\\mbox{CN$_1$CD$_2$E-<<x_1>><<m_2>>}=E\\{Y(1, M_1(<<x_1>>), M_2=<<m_2>>)\\} - E\\{Y(0, M_1(<<x_1>>), M_2=<<m_2>>)\\}$ under Figure 1a.

    \\begin{align*}
    & \\mbox{CN$_1$CD$_2$E-<<x_1>><<m_2>>} \\geq  \\\\\\

    & \\max\\left\\{\\begin{array}{l}
    <<lower>>
      \\end{array}\\right\\},
    \\end{align*}
    \\\\\\
    and
    \\begin{align*}
    & \\mbox{CN$_1$CD$_2$E-<<x_1>><<m_2>>} \\leq \\\\\\

    & \\min\\left\\{\\begin{array}{l}
    <<upper>>
      \\end{array}\\right\\}.
    \\end{align*}", x_1 = x_1, m_2 = m_2, lower = lower, upper = upper,
         .open = "<<", .close = ">>"), "\n")


  }
}

cat(res, file = "cn1-cd2.txt")



res <- NULL
for(m_1 in 0:1) {
for(x_2 in 0:1) {
  for(m_3 in 0:1) {

    raw <- readLines(file.path("bounds-latex", sprintf("cn12cd1e.%s%s%s.txt", m_1, x_2, m_3)))
    lower = paste(raw[c((which(raw == "MAX {")+1):(which(raw == "}")[1]-1))], collapse = "\n")
    upper = paste(raw[c((which(raw == "MIN {")+1):(which(raw == "}")[2]-1))], collapse = "\n")
    res <- paste(res, "\n", glue("\\noindent \\textbf{Result x}:\\\\\\
    The bounds given below are valid and tight for
    $\\mbox{CN$_{12}$CD$_1$E-}<<m_1>><<x_2>><<m_3>>=E\\{Y(1, M_1(<<m_1>>), M_2(X = <<x_2>>,M_1 = <<m_3>>))\\} - E\\{Y(0, M_1(<<m_1>>), M_2(X = <<x_2>>,M_1 = <<m_3>>))\\}$ under Figure 1b.

    \\begin{align*}
    & \\mbox{CN$_{12}$CD$_1$E-<<m_1>><<x_2>><<m_3>>} \\geq  \\\\\\

    & \\max\\left\\{\\begin{array}{l}
    <<lower>>
      \\end{array}\\right\\},
    \\end{align*}
    \\\\\\
    and
    \\begin{align*}
    & \\mbox{CN$_{12}$CD$_1$E-<<m_1>><<x_2>><<m_3>>} \\leq \\\\\\

    & \\min\\left\\{\\begin{array}{l}
    <<upper>>
      \\end{array}\\right\\}.
    \\end{align*}", m_1 = m_1, x_2 = x_2, m_3 = m_3, lower = lower, upper = upper,
                                 .open = "<<", .close = ">>"), "\n")


  }
}}

cat(res, file = "cn12-cd1.txt")



res <- NULL
for(m_1 in 0:1) {
  for(x_2 in 0:1) {
    for(m_3 in 0:1) {

      raw <- readLines(file.path("bounds-latex", sprintf("cn21cd1e.%s%s%s.txt", m_1, x_2, m_3)))
      lower = paste(raw[c((which(raw == "MAX {")+1):(which(raw == "}")[1]-1))], collapse = "\n")
      upper = paste(raw[c((which(raw == "MIN {")+1):(which(raw == "}")[2]-1))], collapse = "\n")
      res <- paste(res, "\n", glue("\\noindent \\textbf{Result x}:\\\\\\
    The bounds given below are valid and tight for
    $\\mbox{CN$_{21}$CD$_1$E-}<<m_1>><<x_2>><<m_3>>=E\\{Y(1, M_1 =<<m_1>>, M_2(X = <<x_2>>,M_1(<<m_3>>)))\\} - E\\{Y(0, M_1 =<<m_1>>, M_2(X = <<x_2>>,M_1(<<m_3>>)))\\}$ under Figure 1b.

    \\begin{align*}
    & \\mbox{CN$_{21}$CD$_1$E-<<m_1>><<x_2>><<m_3>>} \\geq  \\\\\\

    & \\max\\left\\{\\begin{array}{l}
    <<lower>>
      \\end{array}\\right\\},
    \\end{align*}
    \\\\\\
    and
    \\begin{align*}
    & \\mbox{CN$_{21}$CD$_1$E-<<m_1>><<x_2>><<m_3>>} \\leq \\\\\\

    & \\min\\left\\{\\begin{array}{l}
    <<upper>>
      \\end{array}\\right\\}.
    \\end{align*}", m_1 = m_1, x_2 = x_2, m_3 = m_3, lower = lower, upper = upper,
                                   .open = "<<", .close = ">>"), "\n")


    }
  }}

cat(res, file = "cn21-cd1.txt")


res <- NULL
for(m_1 in 0:1) {
  for(x_2 in 0:1) {
    for(m_3 in 0:1) {

      raw <- readLines(file.path("bounds-latex", sprintf("cn2cd11e.%s%s%s.txt", m_1, x_2, m_3)))
      lower = paste(raw[c((which(raw == "MAX {")+1):(which(raw == "}")[1]-1))], collapse = "\n")
      upper = paste(raw[c((which(raw == "MIN {")+1):(which(raw == "}")[2]-1))], collapse = "\n")
      res <- paste(res, "\n", glue("\\noindent \\textbf{Result x}:\\\\\\
    The bounds given below are valid and tight for
    $\\mbox{CN$_{2}$CD$_{11}$E-}<<m_1>><<x_2>><<m_3>>=E\\{Y(1, M_1 =<<m_1>>, M_2(X = <<x_2>>, M_1 = <<m_3>>))\\} - E\\{Y(0, M_1 =<<m_1>>, M_2(X = <<x_2>>, M_1 = <<m_3>>))\\}$ under Figure 1b.

    \\begin{align*}
    & \\mbox{CN$_{2}$CD$_{11}$E-<<m_1>><<x_2>><<m_3>>} \\geq  \\\\\\

    & \\max\\left\\{\\begin{array}{l}
    <<lower>>
      \\end{array}\\right\\},
    \\end{align*}
    \\\\\\
    and
    \\begin{align*}
    & \\mbox{CN$_{2}$CD$_{11}$E-<<m_1>><<x_2>><<m_3>>} \\leq \\\\\\

    & \\min\\left\\{\\begin{array}{l}
    <<upper>>
      \\end{array}\\right\\}.
    \\end{align*}", m_1 = m_1, x_2 = x_2, m_3 = m_3, lower = lower, upper = upper,
                                   .open = "<<", .close = ">>"), "\n")


    }
  }}

cat(res, file = "cn2-cd11.txt")


