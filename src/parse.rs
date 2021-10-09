use std::fs::File;
use std::io::{BufReader, Read};

use TokenKind::*;

use crate::ast::*;
use crate::lex::*;
use crate::log;

#[cfg(test)]
mod parse_test;

fn parse_literal(text: &str) -> Option<Literal> {
    if text == "42" {
        Some(Literal::I32(42))
    } else {
        None
    }
}

fn eat_fn_arg(parser: &mut Parser) -> Option<FnArg> {
    let (one, two) = parser.tokens.peek_two();
    if one.kind == Text && two.kind == Equals {
        parser.tokens.advance();
        parser.tokens.advance();
        if let Some(expr) = parser.eat_expression() {
            Some(FnArg { name: Some(parser.tok_chars(one).to_string()), value: expr })
        } else {
            None
        }
    } else if let Some(expr) = parser.eat_expression() {
        Some(FnArg { name: None, value: expr })
    } else {
        None
    }
}

struct Parser<'a> {
    tokens: Tokens,
    source: &'a str,
}
impl<'a> Parser<'a> {
    fn make(tokens: Tokens, source: &'a str) -> Parser<'a> {
        Parser { tokens, source }
    }
    fn chars_at(&self, start: usize, end: usize) -> &str {
        &self.source[start..end]
    }
    fn tok_chars(&self, tok: Token) -> &str {
        let s = self.chars_at(tok.start, tok.start + tok.len);
        log::verbose(&format!("tok chars {} '{}'", tok.kind, s));
        s
    }

    fn check_eat_token(&mut self, target_token: TokenKind) -> Option<()> {
        let tok = self.tokens.peek();
        log::verbose(&format!("check_eat_token {:?}", tok.kind));
        if tok.kind == target_token {
            self.tokens.next();
            Some(())
        } else {
            None
        }
    }

    fn check_eat_ident(&mut self) -> Option<String> {
        let tok = self.tokens.peek();
        let res = if Text == tok.kind {
            let ident = self.tok_chars(tok);
            Some(String::from(ident))
        } else {
            None
        };
        if res.is_some() {
            self.tokens.advance();
        }
        res
    }

    fn eat_type_expression(&mut self) -> Option<TypeExpression> {
        let tok = self.tokens.peek();
        if let Text = tok.kind {
            let ident = self.tok_chars(tok);
            if ident == "i32" {
                self.tokens.next();
                Some(TypeExpression::Primitive(TypePrimitive::I32))
            } else {
                None
            }
        } else {
            None
        }
    }

    fn check_eat_literal(&mut self) -> Option<Literal> {
        let tok = self.tokens.peek();
        if let Text = tok.kind {
            let text = self.tok_chars(tok);
            parse_literal(text)
        } else {
            None
        }
    }

    fn eat_fn_arg(&mut self) -> Option<FnArg> {
        let (one, two) = self.tokens.peek_two();
        if one.kind == Text && two.kind == Equals {
            self.tokens.advance();
            self.tokens.advance();
            if let Some(expr) = self.eat_expression() {
                Some(FnArg { name: Some(self.tok_chars(one).to_string()), value: expr })
            } else {
                None
            }
        } else if let Some(expr) = self.eat_expression() {
            Some(FnArg { name: None, value: expr })
        } else {
            None
        }
    }

    fn eat_expression(&mut self) -> Option<Expression> {
        let (tok, next) = self.tokens.peek_two();
        if let Text = tok.kind {
            // Literal
            if let Some(lit) = parse_literal(self.tok_chars(tok)) {
                Some(Expression::Literal(lit))
            } else {
                // FnCall
                if next.kind == OpenParen {
                    log::verbose("eat_expression FnCall");
                    if let Some(args) = self.eat_delimited(Comma, eat_fn_arg) {
                        Some(Expression::FnCall(FnCall {
                            name: Ident(self.tok_chars(tok).to_string()),
                            args: args,
                        }))
                    } else {
                        None
                    }
                } else {
                    // Plain Reference
                    Some(Expression::Variable(Ident(self.tok_chars(tok).to_string())))
                }
            }
        } else {
            None
        }
    }

    fn eat_val(&mut self) -> Option<ValDef> {
        let _val = self.check_eat_token(KeywordVal)?;
        let ident = self.check_eat_ident()?;
        let _colon = self.check_eat_token(Colon)?;
        let typ = self.eat_type_expression()?;
        let value = self.eat_expression()?;
        return Some(ValDef {
            name: Ident(ident),
            typ: Some(typ),
            value: value,
        });
    }

    fn eat_fn_arg_def(&mut self) -> Option<FnArgDef> {
        let ident = self.check_eat_ident()?;
        let _colon = self.check_eat_token(Colon)?;
        let typ = self.eat_type_expression()?;
        return Some(FnArgDef {
            name: Ident(ident),
            typ: typ,
            default: None,
        });
    }

    fn eat_fndef_args(&mut self) -> Option<Vec<FnArgDef>> {
        self.eat_delimited(Comma, |p| Parser::eat_fn_arg_def(p))
    }

    fn eat_delimited<T, F>(&mut self, delim: TokenKind, parse: F) -> Option<Vec<T>>
        where F: Fn(&mut Parser) -> Option<T> {
        if let Some(first) = parse(self) {
            // TODO @Allocation Need to figure out how we use all the small vecs without allocating all the time wastefully
            let mut v = Vec::with_capacity(32);
            v.push(first);
            while let Some(parsed) = parse(self) {
                v.push(parsed);
                if self.check_eat_token(CloseParen).is_some() {
                    break;
                } else {
                    let found_delim = self.check_eat_token(delim);
                    if found_delim.is_none() {
                        log::normal("eat_delimited did not find delimiter after 'parse'.");
                        return None;
                    }
                }
            }
            Some(v)
        } else {
            None
        }
    }

    fn eat_block(&mut self) -> Block {
        Block { exprs: Vec::new() }
    }

    fn check_eat_fndef(&mut self) -> Option<FnDef> {
        let _fn = self.check_eat_token(KeywordFn)?;
        let ident = self.check_eat_ident()?;
        let _open_paren = self.check_eat_token(OpenParen)?;
        let args = self.eat_fndef_args()?;
        let _colon = self.check_eat_token(Colon);
        let ret_type = self.eat_type_expression();
        let block = self.eat_block();
        Some(FnDef {
            name: Ident(ident),
            args,
            ret_type: ret_type,
            type_args: None,
            block: block,
        })
    }

    fn eat_definition(&mut self) -> Result<Definition, String> {
        // Perf note: Try the most common things alternates first, or the cheapest to try
        if let Some(val_def) = self.eat_val() {
            Ok(Definition::ValDef(val_def))
        } else if let Some(fn_def) = self.check_eat_fndef() {
            Ok(Definition::FnDef(fn_def))
        } else {
            Err("Expected Const or FnDef; got somethin' else".to_string())
        }

    }

    fn eat_module(&mut self) -> Result<Module, String> {
        let mut defs: Vec<Definition> = vec![];

        while let Ok(def) = self.eat_definition() {
            defs.push(def)
        }
        Ok(Module {
            name: Ident("module1".to_string()),
            defs: defs,
        })
    }
}

fn print_tokens(tokens: &[Token]) {
    let mut line_num = 1;
    for tok in tokens.iter() {
        if tok.line_num > line_num {
            line_num += 1;
            println!()
        }
        print!("{} [{}-{}]", tok.kind, tok.start, tok.start + tok.len);
    }
}

pub fn parse_file(path: &str) -> Result<Module, String> {
    let file = File::open(path).expect(&format!("file not found {}", path));
    let mut buf_read = BufReader::new(file);
    let mut content = String::new();
    buf_read.read_to_string(&mut content);
    let mut lexer = Lexer::make(&content);

    let token_vec = tokenize(&mut lexer);
    print_tokens(&token_vec);

    // TODO @Clone: Just RC the original tokens so everything can look at them
    let mut tokens: Tokens = Tokens::make(token_vec.clone());

    let mut parser = Parser::make(tokens, &content);

    let module = parser.eat_module();

    module
}