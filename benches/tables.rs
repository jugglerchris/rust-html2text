#![feature(test)]
extern crate html2text;
extern crate test;

use ::test::Bencher;

use html2text::from_read;

fn make_html(content: &str) -> String {
    String::from("<html>") + content + "</html>"
}

fn make_tab(cell: &str, rows: usize, cols: usize) -> String {
    let mut result = String::from("<table>");
    for _ in 0..rows {
        result.push_str("<tr>");
        for _ in 0..cols {
            result.push_str("<td>");
            result.push_str(cell);
            result.push_str("</td>");
        }
        result.push_str("</tr>");
    }
    result
}

#[bench]
fn bench_empty(b: &mut Bencher) {
    b.iter(|| from_read(make_html("").as_bytes(), 80));
}

#[bench]
fn bench_tab_1_1(b: &mut Bencher) {
    b.iter(|| from_read(make_html(&make_tab("cell", 1, 1)).as_bytes(), 80));
}
#[bench]
fn bench_tab_2_2(b: &mut Bencher) {
    b.iter(|| from_read(make_html(&make_tab("cell", 2, 2)).as_bytes(), 80));
}
#[bench]
fn bench_tab_3_3(b: &mut Bencher) {
    b.iter(|| from_read(make_html(&make_tab("cell", 3, 3)).as_bytes(), 80));
}
#[bench]
fn bench_tab_4_4(b: &mut Bencher) {
    b.iter(|| from_read(make_html(&make_tab("cell", 4, 4)).as_bytes(), 80));
}
#[bench]
fn bench_tab_5_5(b: &mut Bencher) {
    b.iter(|| from_read(make_html(&make_tab("cell", 5, 5)).as_bytes(), 80));
}
#[bench]
fn bench_tab_6_6(b: &mut Bencher) {
    b.iter(|| from_read(make_html(&make_tab("cell", 6, 6)).as_bytes(), 80));
}
// Try a table with `depth` nested tables each with `rows` rows and `cols` columns.
fn bench_tab_depth(b: &mut Bencher, content: &str, depth: usize, rows: usize, cols: usize) {
    let mut t = String::from(content);
    for _ in 0..depth {
        t = make_tab(&t, rows, cols);
    }
    let html = make_html(&t);
    b.iter(|| from_read(html.as_bytes(), 80));
}
#[bench]
fn bench_tab_2_1_depth_2(b: &mut Bencher) {
    bench_tab_depth(b, "cell", 2, 2, 1);
}
#[bench]
fn bench_tab_3_1_depth_2(b: &mut Bencher) {
    bench_tab_depth(b, "cell", 2, 3, 1);
}
#[bench]
fn bench_tab_4_1_depth_2(b: &mut Bencher) {
    bench_tab_depth(b, "cell", 2, 4, 1);
}
#[bench]
fn bench_tab_1_2_depth_2(b: &mut Bencher) {
    bench_tab_depth(b, "cell", 2, 1, 2);
}
#[bench]
fn bench_tab_1_3_depth_2(b: &mut Bencher) {
    bench_tab_depth(b, "cell", 2, 1, 3);
}
#[bench]
fn bench_tab_1_4_depth_2(b: &mut Bencher) {
    bench_tab_depth(b, "cell", 2, 1, 4);
}
#[bench]
fn bench_tab_2_depth_2(b: &mut Bencher) {
    bench_tab_depth(b, "cell", 2, 2, 2);
}
/*
#[bench]
fn bench_tab_2_depth_3(b: &mut Bencher) {
    bench_tab_depth(b, "cell", 3, 2, 2);
}
#[bench]
fn bench_tab_2_depth_4(b: &mut Bencher) {
    bench_tab_depth(b, "cell", 4, 2, 2);
}
#[bench]
fn bench_tab_2_depth_5(b: &mut Bencher) {
    bench_tab_depth(b, "cell", 5, 2, 2);
}
*/
