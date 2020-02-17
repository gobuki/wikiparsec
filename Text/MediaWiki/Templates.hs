-- `Text.MediaWiki.Templates`: representing and applying templates
-- ===============================================================

{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Text.MediaWiki.Templates where
import WikiPrelude
import Text.MediaWiki.AnnotatedText

-- Parsing templates the same way they're parsed on Wikipedia or Wiktionary would
-- be an insanely complicated and time-consuming process, as their actions are
-- interpreted from an ad-hoc programming language *written in Wikitext* that
-- itself has to be parsed. On top of that, some of them run PHP or Lua code via
-- extensions.

-- We assume here that we don't want to be able to fill in every template; we just
-- want to output something reasonable from the most common templates, and in most
-- cases output nothing at all.

-- First: the syntax of a template is represented as an association list from
-- parameter names to values.  Both the names and the values are Text.

-- To customize the values of templates for different wikis, we'll be passing
-- around an object called a TemplateProc, which looks up the name of the template
-- and returns a way to manipulate the text. That is:

-- - It takes in a Text, the same one that's in the "0" slot of the
--   template. (We pass this as a separate argument to make dispatch much
--   easier, because any non-trivial computation you do will depend on the name of
--   the template.)

-- - It returns a function that takes an Annotation of the template's arguments
--   and returns an AnnotatedText, which we're calling a TemplateAction.

type Template = [(Text, Text)]
type TemplateAction = (Template -> AnnotatedText)
type TemplateProc = Text -> TemplateAction

-- The simplest TemplateProc is `ignoreTemplates`, which returns the empty
-- AnnotatedText for any template.

ignoreTemplates :: TemplateProc
ignoreTemplates = const skipTemplate

-- Generally useful template actions
-- ---------------------------------

-- `skipTemplate` outputs the empty string no matter what the arguments of the
-- template are.

-- `idTemplate` returns the name of the template as its value.

-- `useArg` returns a given named or positional argument.

-- Keep in mind that template arguments are always Text, even the positional
-- ones such as "1". We do this to keep types consistent as we emulate PHP.

skipTemplate :: TemplateAction
skipTemplate = const ø

useArg :: Text -> TemplateAction
useArg arg = annotFromText . (get arg)
idTemplate = useArg "0"

-- When we parse a template, we get a result of type `Template`. To actually
-- evaluate it, we extract its template name (its 0th argument), pass that
-- template name to `tproc` to get a function that specifies what to do, and apply
-- that function to the `Template` structure.

evalTemplate :: TemplateProc -> Template -> AnnotatedText
evalTemplate tproc tdata =
  let action = tproc (get "0" tdata) in action tdata
-- 