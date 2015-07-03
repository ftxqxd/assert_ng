#![feature(plugin)]

#![plugin(assert_ng)]

#[test]
#[should_panic(expected = "foo bar")]
fn assert_with_message() {
    assert_ng!(1 != 1, "foo bar");
}

#[test]
#[should_panic(expected = "assertion failed: 1 + 1 == 1:
left:  `2`
right: `1`")]
fn simple_eq() {
    assert_ng!(1 + 1 == 1);
}

#[test]
#[should_panic(expected = "assertion failed: 1 /* foo */ + 1 /* bar */ == 1:
left:  `2`
right: `1`")]
fn comments() {
    assert_ng!(  1 // foo
               + 1 // bar
               == 1);
}

#[test]
fn complex_expressions() {
    assert_ng!(1 + 1 == 2);
    assert_ng!(1 + 1 != 1);
    assert_ng!(1 + 1 >  1);
    assert_ng!(1 + 1 >= 2);
    assert_ng!(1 + 1 <  3);
    assert_ng!(1 + 1 <= 3);
    assert_ng!(true && true);
}
