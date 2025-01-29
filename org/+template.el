;;; pimacs/org/+template.el -*- lexical-binding: t; -*-

(setq org-latex-pdf-process
      '("latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -f %f"))

(unless (boundp 'org-latex-classes)
  (setq org-latex-classes nil))

(when (modulep! +lang-fr)
  ;; Adpapded from https://github.com/GeneKao/orgmode-latex-templates
  (add-to-list 'org-latex-classes
               '("pim-fr-article-1"
                 "\\documentclass[10pt,a4paper]{article}
\\usepackage[utf8]{inputenc}
\\usepackage[a4paper, margin={2cm,2cm}]{geometry}
\\usepackage[T1]{fontenc}
\\usepackage[french]{babel}
\\usepackage{lmodern}
\\usepackage{parskip}
\\usepackage{graphicx}
\\usepackage{longtable}
\\usepackage{float}
\\usepackage{wrapfig}
\\usepackage{rotating}
\\usepackage[normalem]{ulem}
\\usepackage{amsmath,amsfonts,mathtools,amsthm,amssymb}
\\usepackage{textcomp}
\\usepackage{marvosym}
\\usepackage{wasysym}
\\usepackage[hyperindex]{hyperref}
\\usepackage[hyperref]{xcolor}
\\definecolor{brick}{HTML}{7B0C00}
\\usepackage{color}
\\usepackage{enumerate}
\\definecolor{bg}{rgb}{0.95,0.95,0.95}
\\tolerance=1000
      [NO-DEFAULT-PACKAGES]
      [PACKAGES]
      [EXTRA]
\\linespread{1.2}
\\hypersetup{
  colorlinks=true,
  filecolor=red,
  linkcolor=brick,urlcolor=blue,
  %pdfstartview=Fit,
  pdfauthor={Philippe Ivaldi},
  pdfcreator={pdfLaTeX and Emacs}
  pdfborder=0 0 0
}

\\usepackage{enumitem}
\\setlist[itemize,1]{label=\\textbullet}
\\setlist[itemize,2]{label=$\\circ$}
\\setlist[itemize,3]{label=$\\ast$}
"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")))
  )


;; Comes from https://github.com/GeneKao/orgmode-latex-templates
(add-to-list 'org-latex-classes
             '("ebook"
               "\\documentclass[11pt, oneside]{memoir}
\\setstocksize{9in}{6in}
\\settrimmedsize{\\stockheight}{\\stockwidth}{*}
\\setlrmarginsandblock{2cm}{2cm}{*} % Left and right margin
\\setulmarginsandblock{2cm}{2cm}{*} % Upper and lower margin
\\checkandfixthelayout
% Much more laTeX code omitted
"
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")))
