--[[
--    nvim theme inspired by photon, but effectively inverted
--
--    I have next to no interest in figuring out the faces
--    manually, so much of these were spit out by an LLM.
--    As a result, it's entirely possible that some of them
--    are total bullshit.
--]]

vim.cmd("hi clear")
if vim.fn.exists("syntax_on") == 1 then
  vim.cmd("syntax reset")
end

vim.o.termguicolors = true
vim.o.background    = "dark"
vim.g.colors_name   = "purple"

-- Palette
local c = {
  bg         = "#000000",
  fg         = "#ffffff",
  dim        = "#999999",
  comment    = "#666666",
  purple     = "#af87d8",
  region_bg  = "#4a2a6d",
  preproc    = "#696969",
  string     = "#BBBBBB",
  red        = "#ff0000",
}

local hl = vim.api.nvim_set_hl

hl(0, "Normal",                 { fg = c.fg, bg = c.bg })
hl(0, "NonText",                { fg = c.dim })
hl(0, "Comment",                { fg = c.comment })
hl(0, "Conceal",                { fg = c.comment })
hl(0, "ModeMsg",                { fg = c.purple })
hl(0, "Keyword",                { fg = c.purple })
hl(0, "Statement",              { fg = c.purple })
hl(0, "Conditional",            { fg = c.purple })
hl(0, "Repeat",                 { fg = c.purple })
hl(0, "Label",                  { fg = c.dim })
hl(0, "Exception",              { fg = c.purple })
hl(0, "Function",               { fg = c.fg })
hl(0, "Type",                   { fg = c.purple })
hl(0, "StorageClass",           { link = "Type" })
hl(0, "Structure",              { link = "Type" })
hl(0, "Typedef",                { link = "Type" })
hl(0, "Identifier",             { fg = c.fg })
hl(0, "Constant",               { fg = c.dim })
hl(0, "Number",                 { link = "Constant" })
hl(0, "Boolean",                { link = "Constant" })
hl(0, "Float",                  { link = "Constant" })
hl(0, "String",                 { fg = c.string })
hl(0, "Character",              { link = "String" })
hl(0, "PreProc",                { fg = c.preproc })
hl(0, "Include",                { link = "PreProc" })
hl(0, "Define",                 { link = "PreProc" })
hl(0, "Macro",                  { link = "PreProc" })
hl(0, "PreCondit",              { link = "PreProc" })
hl(0, "Special",                { fg = c.dim })
hl(0, "SpecialChar",            { link = "Special" })
hl(0, "Delimiter",              { link = "Special" })
hl(0, "SpecialComment",         { link = "Special" })
hl(0, "Debug",                  { link = "Special" })
hl(0, "Error",                  { fg = c.red })
hl(0, "ErrorMsg",               { link = "Error" })
hl(0, "WarningMsg",             { fg = c.red })
hl(0, "Todo",                   { fg = c.purple })
hl(0, "Visual",                 { bg = c.region_bg, fg = c.fg })
hl(0, "VisualNOS",              { bg = c.region_bg })
hl(0, "CursorLine",             { bg = "#222222" })
hl(0, "CursorColumn",           { link = "CursorLine" })
hl(0, "Search",                 { bg = c.region_bg, fg = c.fg })
hl(0, "IncSearch",              { bg = c.region_bg, fg = c.fg })
hl(0, "MatchParen",             { bg = nil, fg = c.purple })
hl(0, "LspReferenceRead",       { bg = c.region_bg })
hl(0, "LspReferenceText",       { bg = c.region_bg })
hl(0, "LspReferenceWrite",      { bg = c.region_bg })
hl(0, "LineNr",                 { fg = c.purple })
hl(0, "LineNrAbove",            { fg = c.dim })
hl(0, "LineNrBelow",            { fg = c.dim })
hl(0, "StatusLine",             { fg = c.purple, bg = "#222222" })
hl(0, "StatusLineNC",           { fg = c.dim, bg = "#111111" })
hl(0, "VertSplit",              { fg = c.bg, bg = c.bg })
hl(0, "Pmenu",                  { fg = c.fg, bg = "#222222" })
hl(0, "PmenuSel",               { fg = c.fg, bg = c.region_bg })
hl(0, "PmenuSbar",              { bg = c.purple })
hl(0, "PmenuThumb",             { bg = c.purple })
hl(0, "NormalFloat",            { bg = "#222222" })
hl(0, "TelescopeNormal",        { bg = "#151515" })
hl(0, "Directory",              { fg = c.purple })
hl(0, "Title",                  { fg = c.purple })
hl(0, "Question",               { fg = c.purple })
hl(0, "@keyword",               { link = "Keyword" })
hl(0, "@keyword.function",      { link = "Keyword" })
hl(0, "@keyword.operator",      { link = "Keyword" })
hl(0, "@keyword.return",        { link = "Keyword" })
hl(0, "@function",              { link = "Function" })
hl(0, "@function.builtin",      { link = "Function" })
hl(0, "@method",                { link = "Function" })
hl(0, "@constructor",           { link = "Function" })
hl(0, "@type",                  { link = "Type" })
hl(0, "@type.builtin",          { link = "Type" })
hl(0, "@type.definition",       { link = "Type" })
hl(0, "@variable",              { link = "Identifier" })
hl(0, "@variable.builtin",      { link = "Identifier" })
hl(0, "@parameter",             { link = "Identifier" })
hl(0, "@field",                 { link = "Identifier" })
hl(0, "@property",              { link = "Identifier" })
hl(0, "@constant",              { link = "Constant" })
hl(0, "@constant.builtin",      { link = "Constant" })
hl(0, "@number",                { link = "Constant" })
hl(0, "@boolean",               { link = "Constant" })
hl(0, "@string",                { link = "String" })
hl(0, "@string.escape",         { link = "String" })
hl(0, "@string.special",        { link = "String" })
hl(0, "@comment",               { link = "Comment" })
hl(0, "@comment.todo",          { link = "Todo" })
hl(0, "@operator",              { link = "Special" })
hl(0, "@punctuation",           { link = "Special" })
hl(0, "@punctuation.delimiter", { link = "Special" })
hl(0, "@punctuation.bracket",   { link = "Special" })
hl(0, "@preproc",               { link = "PreProc" })
hl(0, "@define",                { link = "PreProc" })
hl(0, "@macro",                 { link = "PreProc" })
hl(0, "@markup.heading",        { link = "Title" })
hl(0, "@markup.link",           { link = "Directory" })
hl(0, "@markup.strong",         { link = "Identifier" })
hl(0, "@markup.emphasis",       { link = "Identifier" })
hl(0, "@markup.literal",        { link = "String" })
