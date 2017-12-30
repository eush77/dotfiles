function wraplines --description 'Wrap long lines at 72 characters. Expects one space between words, 2 NL between paragraphs'
        tr '\n' ' ' |sed 's:  :\n\n:g' |fold -sw 72
end
