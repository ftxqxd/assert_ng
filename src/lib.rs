#![crate_type="dylib"]
#![feature(plugin_registrar, quote)]

extern crate syntax;
extern crate rustc;

use syntax::codemap::{DUMMY_SP, Span};
use syntax::parse::token;
use syntax::ast::{mod, TokenTree};
use syntax::ptr::P;
use syntax::ext::base::{ExtCtxt, MacResult, MacExpr};
use rustc::plugin::Registry;

fn expand_assert_ng(cx: &mut ExtCtxt, sp: Span, args: &[TokenTree])
        -> Box<MacResult + 'static> {
    expand_assert_ng_(cx, sp, args, false)
}

fn expand_debug_assert_ng(cx: &mut ExtCtxt, sp: Span, args: &[TokenTree])
        -> Box<MacResult + 'static> {
    expand_assert_ng_(cx, sp, args, true)
}

fn expand_assert_ng_(cx: &mut ExtCtxt, _: Span, args: &[TokenTree], debug_only: bool)
        -> Box<MacResult + 'static> {
    let mut parser = cx.new_parser_from_tts(args);

    let expr: P<ast::Expr> = parser.parse_expr();

    let res = if parser.eat(&token::Comma) {
        let ts = parser.parse_all_token_trees();
        let pth = ast::Path {
            span: DUMMY_SP,
            global: false,
            segments: vec![
                ast::PathSegment {
                    identifier: ast::Ident::new(token::intern("panic")),
                    parameters: ast::AngleBracketedParameters(ast::AngleBracketedParameterData {
                        lifetimes: vec![],
                        types: syntax::owned_slice::OwnedSlice::empty(),
                    }),
                }
            ],
        };
        parser.mk_mac_expr(expr.span.lo, expr.span.hi, ast::MacInvocTT(pth, ts, ast::EMPTY_CTXT))
    } else {
        match expr.node {
            ast::ExprBinary(ast::BiEq, ref given, ref expected) => {
                quote_expr!(cx,
                    match (&($given), &($expected)) {
                        (given_val, expected_val) => {
                            if !(*given_val == *expected_val) {
                                panic!("assertion failed: left == right:\n\
                                        left:  `{}`\n\
                                        right: `{}`", *given_val, *expected_val);
                            }
                        }
                    }
                )
            },
            ast::ExprBinary(ast::BiNe, ref given, ref expected) => {
                quote_expr!(cx,
                    match (&($given), &($expected)) {
                        (given_val, expected_val) => {
                            if !(*given_val != *expected_val) {
                                panic!("assertion failed: left != right:\n\
                                        left:  `{}`\n\
                                        right: `{}`", *given_val, *expected_val);
                            }
                        }
                    }
                )
            },
            ast::ExprBinary(ast::BiGt, ref given, ref expected) => {
                quote_expr!(cx,
                    match (&($given), &($expected)) {
                        (given_val, expected_val) => {
                            if !(*given_val > *expected_val) {
                                panic!("assertion failed: left > right:\n\
                                        left:  `{}`\n\
                                        right: `{}`", *given_val, *expected_val);
                            }
                        }
                    }
                )
            },
            ast::ExprBinary(ast::BiLt, ref given, ref expected) => {
                quote_expr!(cx,
                    match (&($given), &($expected)) {
                        (given_val, expected_val) => {
                            if !(*given_val < *expected_val) {
                                panic!("assertion failed: left < right:\n\
                                        left:  `{}`\n\
                                        right: `{}`", *given_val, *expected_val);
                            }
                        }
                    }
                )
            },
            ast::ExprBinary(ast::BiGe, ref given, ref expected) => {
                quote_expr!(cx,
                    match (&($given), &($expected)) {
                        (given_val, expected_val) => {
                            if !(*given_val >= *expected_val) {
                                panic!("assertion failed: left >= right:\n\
                                        left:  `{}`\n\
                                        right: `{}`", *given_val, *expected_val);
                            }
                        }
                    }
                )
            },
            ast::ExprBinary(ast::BiLe, ref given, ref expected) => {
                quote_expr!(cx,
                    match (&($given), &($expected)) {
                        (given_val, expected_val) => {
                            if !(*given_val <= *expected_val) {
                                panic!("assertion failed: left <= right:\n\
                                        left:  `{}`\n\
                                        right: `{}`", *given_val, *expected_val);
                            }
                        }
                    }
                )
            },
            _ => {
                quote_expr!(cx,
                    if !($expr) {
                        panic!(concat!("assertion failed: ", stringify!($expr)));
                    }
                )
            }
        }
    };

    MacExpr::new(if debug_only {
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
