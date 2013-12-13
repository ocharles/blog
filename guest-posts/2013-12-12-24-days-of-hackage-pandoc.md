---
title: 24 Days of Hackage: pandoc
---

Today I have the pleasure of sitting back and allowing Ron (aka latermuse) to run the
show. Ron approached me almost immediately after I announced 24 Days of Hackage
this year, and has produced a great post about document conversions in
Haskell. So, without further ado, Ron - it's over to you!

---

Back in the days of yore, conversion between document types was nigh impossible
for laymen. Apprentices were historically tasked with conversion but many
inevitably failed due to absymal documentation and a lack of specialized tools.
For charset conversion, we had `iconv`. For media conversion, we had
`ffmpeg`. For document conversion we had nothing. Then came pandoc.

[Pandoc](http://johnmacfarlane.net/pandoc/) quickly filled the void, providing a
simple and painless tool for converting documents between many different
formats. Have you ever written a
[Markdown](http://daringfireball.net/projects/markdown/) document, but later
realized it should have been written with
[LaTeX](http://www.latex-project.org/)?  Pandoc has your back. With a quick
terminal one-liner, your Markdown file has been automatically converted into
LaTeX and the day is saved. Pandoc is the peerless document conversion
multi-tool of your dreams. Whether you are commanding Pandoc through the sleek
command-line interface, or effortlessly integrating the Haskell library into
your own code, there really is no need to look further for a document converter.

Through [John McFarlane's](http://johnmacfarlane.net/) genius division of
labor, Pandoc has separated the conversion of documents into readers and
writers. These readers and writers can be interchanged depending on the source
and target of your document conversion formats. This allows for a simple library
api which allows for a utalitarian "plug and play" interface for conversion.

Pandoc can be used both as an executable or a library. The library can be
imported into Haskell code, letting you apply further customizations. To do so,
simply install Pandoc with `cabal install pandoc`, and then import it as you
would any other library. It's so easy and happy-go-lucky. Let's walk through an
example.

First, let's import the Pandoc text library.

```haskell
import Text.Pandoc
```

Then lets define some Markdown text that we can work with.

```haskell
textToConvert = unlines [ "Hello World.\n"
                        , "    this is a Markdown code block\n"
                        , "[This is a link](http://www.latermuse.com/)\n" ]
```

Next, we need to convert this Markdown into Pandoc's internal format. We can do
this with `readMarkdown`. `readMarkdown` takes configuration parameters, but we
will just use the default settings by supplying `def`.

```haskell
pandocParsed = readMarkdown def textToConvert
```

Pandoc has parsed our Markdown document, and converted it into the Pandoc native
representation data type, ready to be converted into another document type. Here
is what the data type looks like from our example:

```
> print pandocParsed
Pandoc 
  ( Meta 
    { docTitle = []
    , docAuthors = []
    , docDate = [] } )
  [ Para 
    [ Str "Hello",Space,Str "World." ]
    , CodeBlock ("",[],[]) "this is a Markdown code block"
    , Para 
        [ Link 
          [ Str "Here",Space,Str "is",Space,Str "a",Space,Str "url" ] 
          ( "http://www.latermuse.com/","" ) ] ]
```

Now that Pandoc has parsed our document, we can convert it into any document
format that Pandoc supports. Let's use the default options and convert it into
LaTeX:

```haskell
pandocConverted = writeLaTeX def pandocParsed
```

Pandoc will convert the internal representation to a string that contains the
same document, but formatted as LaTeX. We can confirm this by printing the
`pandocConverted` value:

```
> putStrLn pandocConverted
Hello World.

\begin{verbatim}
this is a Markdown code block
\end{verbatim}

\href{http://www.latermuse.com/}{Here is a url}
```

Now you can output this converted String to stdout, write it to a file, or do
whatever you want with it - don't worry, Pandoc won't get jealous. One of the
neat things of Pandoc's native representation data type is that you can now go
back to your parsed document and convert it into a multitude of file types.

```haskell
-- Converts the document to HTML
convertedToHtml = writeHtml def pandocParsed
```

Pandoc also has a lot of configurable options. The following example sets the
column width of the output to 80, enables text wrapping, and converts the
document to the [OpenDocument](http://www.opendocumentformat.org/) format. You
can find a bunch more options on the
[Hackage page](http://hackage.haskell.org/package/pandoc-1.12.1/docs/Text-Pandoc-Options.html#t:WriterOptions).

```haskell
-- Converts the document to the OpenDocument format 
convertedToOpenDocument = writeOpenDocument opts pandocParsed
  where
    opts = def { writerWrapText = True -- Enable text wrapping
               , writerColumns = 80 }  -- Set column width to 80
```

After you are used to the Pandoc workflow, you can start using it for other
things, like automatic citations and bibliographies, templating, slideshow
generation, or scripting. If you are the motivated type, you can even help the
project by coding new reader and writer modules. The sky is the limit with this
simple yet powerful tool.
