#![crate_type="dylib"]
#![feature(plugin_registrar, quote, rustc_private)]

extern crate syntax;
extern crate rustc;

use syntax::codemap::{DUMMY_SP, Span};
use syntax::parse::{self, token, lexer};
use syntax::parse::lexer::Reader;
use syntax::ast::{self, TokenTree};
use syntax::ptr::P;
use syntax::ext::base::{ExtCtxt, MacResult, MacEager, DummyResult};
use syntax::ext::build::AstBuilder;
use rustc::plugin::Registry;

fn expand_assert_ng(cx: &mut ExtCtxt, sp: Span, args: &[TokenTree])
        -> Box<MacResult + 'static> {
    expand_assert_ng_(cx, sp, args, false)
}

fn expand_debug_assert_ng(cx: &mut ExtCtxt, sp: Span, args: &[TokenTree])
        -> Box<MacResult + 'static> {
    expand_assert_ng_(cx, sp, args, true)
}

macro_rules! try_parse {
    ($sp: expr, $e: expr) => {
        match $e.ok() {
            Some(e) => e,
            None => return DummyResult::any($sp),
        }
    }
}

fn expand_assert_ng_(cx: &mut ExtCtxt, sp: Span, args: &[TokenTree], debug_only: bool)
        -> Box<MacResult + 'static> {
    let mut parser = cx.new_parser_from_tts(args);

    let expr: P<ast::Expr> = parser.parse_expr();

    let res = if try_parse!(sp, parser.eat(&token::Comma)) {
        let lo = parser.span.lo;
        let ts = try_parse!(sp, parser.parse_all_token_trees());
        let hi = parser.span.lo; // Not so sure about this
        let pth = ast::Path {
            span: DUMMY_SP,
            global: false,
            segments: vec![
                ast::PathSegment {
                    identifier: ast::Ident::new(token::intern("panic")),
                    parameters: ast::AngleBracketedParameters(ast::AngleBracketedParameterData {
                        lifetimes: vec![],
                        bindings: syntax::owned_slice::OwnedSlice::empty(),
                        types: syntax::owned_slice::OwnedSlice::empty(),
                    }),
                }
            ],
        };
        let span = expr.span;
        let mac = parser.mk_mac_expr(lo, hi, ast::MacInvocTT(pth, ts, ast::EMPTY_CTXT));
        let cond = parser.mk_expr(span.lo, span.hi, ast::ExprUnary(ast::UnNot, expr));
        parser.mk_expr(span.lo, span.hi,
            ast::ExprIf(cond,
                P(ast::Block {
                    stmts: vec![],
                    expr: Some(mac),
                    id: ast::DUMMY_NODE_ID,
                    rules: ast::DefaultBlock,
                    span: span,
                }), None))
    } else {
        let cm = &cx.parse_sess.span_diagnostic.cm;
        let expr_span_string = cm.span_to_snippet(expr.span).unwrap();

        let sess = parse::new_parse_sess();
        let fm = parse::string_to_filemap(&sess, expr_span_string, "<stdin>".to_string());
        let mut lexer = lexer::StringReader::new(&sess.span_diagnostic, fm);

        let mut expr_to_string = String::new();

        loop {
            let next = lexer.next_token();

            let string = sess.span_diagnostic.cm.span_to_snippet(next.sp).unwrap();

            let stringified = match next.tok {
                token::Eof => break,
                token::Comment => {
                    format!("/* {} */", string.trim_left_matches('/').trim())
                },
                _ => {
                    string
                }
            };

            expr_to_string.push_str(&*stringified);
        }

        let mut expr_span_string = String::new();
        for (i, l) in expr_to_string.lines().enumerate() {
            if i != 0 {
                expr_span_string.push(' ');
                expr_span_string.push_str(l.trim());
            } else {
                expr_span_string.push_str(l.trim());
            }
        }

        let expr_to_string_expr =
            cx.expr_str(expr.span, token::intern_and_get_ident(expr_span_string.trim()));
        match expr.node {
            ast::ExprBinary(ref op, ref given, ref expected) => match op.node {
                ast::BiEq => {
                    quote_expr!(cx,
                        match (&($given), &($expected)) {
                            (given_val, expected_val) => {
                                if !(*given_val == *expected_val) {
                                    panic!("assertion failed: {}:\n\
                                            left:  `{}`\n\
                                            right: `{}`",
                                           $expr_to_string_expr,
                                           *given_val,
                                           *expected_val);
                                }
                            }
                        }
                    )
                },
                ast::BiNe => {
                    quote_expr!(cx,
                        match (&($given), &($expected)) {
                            (given_val, expected_val) => {
                                if !(*given_val != *expected_val) {
                                    panic!("assertion failed: {}:\n\
                                            left:  `{}`\n\
                                            right: `{}`",
                                           $expr_to_string_expr,
                                           *given_val,
                                           *expected_val);
                                }
                            }
                        }
                    )
                },
                ast::BiGt => {
                    quote_expr!(cx,
                        match (&($given), &($expected)) {
                            (given_val, expected_val) => {
                                if !(*given_val > *expected_val) {
                                    panic!("assertion failed: {}:\n\
                                            left:  `{}`\n\
                                            right: `{}`",
                                           $expr_to_string_expr,
                                           *given_val,
                                           *expected_val);
                                }
                            }
                        }
                    )
                },
                ast::BiLt => {
                    quote_expr!(cx,
                        match (&($given), &($expected)) {
                            (given_val, expected_val) => {
                                if !(*given_val < *expected_val) {
                                    panic!("assertion failed: {}:\n\
                                            left:  `{}`\n\
                                            right: `{}`",
                                           $expr_to_string_expr,
                                           *given_val,
                                           *expected_val);
                                }
                            }
                        }
                    )
                },
                ast::BiGe => {
                    quote_expr!(cx,
                        match (&($given), &($expected)) {
                            (given_val, expected_val) => {
                                if !(*given_val >= *expected_val) {
                                    panic!("assertion failed: {}:\n\
                                            left:  `{}`\n\
                                            right: `{}`",
                                           $expr_to_string_expr,
                                           *given_val,
                                           *expected_val);
                                }
                            }
                        }
                    )
                },
                ast::BiLe => {
                    quote_expr!(cx,
                        match (&($given), &($expected)) {
                            (given_val, expected_val) => {
                                if !(*given_val <= *expected_val) {
                                    panic!("assertion failed: {}:\n\
                                            left:  `{}`\n\
                                            right: `{}`",
                                           $expr_to_string_expr,
                                           *given_val,
                                           *expected_val);
                                }
                            }
                        }
                    )
                },
                _ => {
                    quote_expr!(cx,
                        if !($expr) {
                            panic!(concat!("assertion failed: ", $expr_to_string_expr));
                        }
                    )
                }
            },
            _ => {
                quote_expr!(cx,
                    if !($expr) {
                        panic!(concat!("assertion failed: ", $expr_to_string_expr));
                    }
                )
            }
        }
    };

    MacEager::expr(if debug_only {
        quote_expr!(cx, if cfg!(not(ndebug)) { $res })
    } else {
        res
    })
}

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
    reg.register_macro("assert_ng", expand_assert_ng);
    reg.register_macro("debug_assert_ng", expand_debug_assert_ng);
}
