# haskell-language-hcl
**`language-hcl`** contains HCL (Hashicorp Configuration Language) and `.conf`
(e.g. nginx configuration) parsers and pretty-printers for the Haskell
programming language.

- `Data.HCL` exports the HCL parser
- `Data.HCL.PrettyPrint` exports the HCL pretty-printer
- `Data.Conf` exports the `.conf` parser
- `Data.Conf.PrettyPrint` exports the `.conf` pretty-printer
- `ConfFmt` is a `.conf` file formatter that serves as an example; it's built as
  `conffmt` by the cabal configuration

## License
Published under the GPLv3
